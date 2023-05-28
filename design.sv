// block processor
module sha256_block (
    input clk, 
  input [255:0] H_in,//after initial hash value
  input [511:0] M_in,// the massage
    input input_valid,
  output [255:0] H_out,//the hash in the end
    output output_valid
    );

reg [6:0] round;
  wire [31:0] a_in = H_in[255:224], b_in = H_in[223:192], c_in = H_in[191:160], d_in = H_in[159:128]; //a=h1 or b=h2 and etc
wire [31:0] e_in = H_in[127:96], f_in = H_in[95:64], g_in = H_in[63:32], h_in = H_in[31:0];
  reg [31:0] a_c, b_c, c_c, d_c, e_c, f_c, g_c, h_c;// current value 
  wire [31:0] a_e, b_e, c_e, d_e, e_e, f_e, g_e, h_e;//computed end value 
wire [31:0] W_tm2, W_tm15, s1_Wtm2, s0_Wtm15, Wj, Kj;
assign H_out = {
    a_in + a_c, b_in + b_c, c_in + c_c, d_in + d_c, e_in + e_c, f_in + f_c, g_in + g_c, h_in + h_c
};// the final result is like h1=h1(i-1)+a
assign output_valid = round == 64;//sha256 need a 64 rounds to be done and ready
//
always @(posedge clk)
begin
  if (input_valid) begin// in the start 
        a_c <= a_in; b_c <= b_in; c_c <= c_in; d_c <= d_in;
        e_c <= e_in; f_c <= f_in; g_c <= g_in; h_c <= h_in;
        round <= 0;
    end else begin
        a_c <= a_e; b_c <= b_e; c_c <= c_e; d_c <= d_e;
        e_c <= e_e; f_c <= f_e; g_c <= g_e; h_c <= h_e;
        round <= round + 1;
    end
end
// round 
sha256_round sha256_round (
    .Kj(Kj), .Wj(Wj),
    .a_in(a_c), .b_in(b_c), .c_in(c_c), .d_in(d_c),
    .e_in(e_c), .f_in(f_c), .g_in(g_c), .h_in(h_c),
    .a_out(a_e), .b_out(b_e), .c_out(c_e), .d_out(d_e),
    .e_out(e_e), .f_out(f_e), .g_out(g_e), .h_out(h_e)
);
  //end
sha256_s0 sha256_s0 (.x(W_tm15), .s0(s0_Wtm15));
sha256_s1 sha256_s1 (.x(W_tm2), .s1(s1_Wtm2));

W_machine #(.WORDSIZE(32)) W_machine (
    .clk(clk),
    .M(M_in), .M_valid(input_valid),
    .W_tm2(W_tm2), .W_tm15(W_tm15),
    .s1_Wtm2(s1_Wtm2), .s0_Wtm15(s0_Wtm15),
    .W(Wj)
);

sha256_K_machine sha256_K_machine (
    .clk(clk), .rst(input_valid), .K(Kj)
);

endmodule


// round computation function start 
module sha256_round (
    input [31:0] Kj, Wj,
  input [31:0] a_in, b_in, c_in, d_in, e_in, f_in, g_in, h_in,
    output [31:0] a_out, b_out, c_out, d_out, e_out, f_out, g_out, h_out
);//a_in here is a_c current value

wire [31:0] Ch_e_f_g, Maj_a_b_c, S0_a, S1_e;

Ch #(.WORDSIZE(32)) Ch (
    .x(e_in), .y(f_in), .z(g_in), .Ch(Ch_e_f_g)
);

Maj #(.WORDSIZE(32)) Maj (
    .x(a_in), .y(b_in), .z(c_in), .Maj(Maj_a_b_c)
);

sha256_S0 S0 (
    .x(a_in), .S0(S0_a)
);

sha256_S1 S1 (
    .x(e_in), .S1(S1_e)
);

sha2_round #(.WORDSIZE(32)) sha256_round_inner (
    .Kj(Kj), .Wj(Wj),
    .a_in(a_in), .b_in(b_in), .c_in(c_in), .d_in(d_in),
    .e_in(e_in), .f_in(f_in), .g_in(g_in), .h_in(h_in),
    .Ch_e_f_g(Ch_e_f_g), .Maj_a_b_c(Maj_a_b_c), .S0_a(S0_a), .S1_e(S1_e),
    .a_out(a_out), .b_out(b_out), .c_out(c_out), .d_out(d_out),
    .e_out(e_out), .f_out(f_out), .g_out(g_out), .h_out(h_out)
);

endmodule


// Σ₀(x)
module sha256_S0 (
    input wire [31:0] x,
    output wire [31:0] S0
    );

assign S0 = ({x[1:0], x[31:2]} ^ {x[12:0], x[31:13]} ^ {x[21:0], x[31:22]});

endmodule


// Σ₁(x)
module sha256_S1 (
    input wire [31:0] x,
    output wire [31:0] S1
    );

assign S1 = ({x[5:0], x[31:6]} ^ {x[10:0], x[31:11]} ^ {x[24:0], x[31:25]});

endmodule


// σ₀(x)
module sha256_s0 (
    input wire [31:0] x,
    output wire [31:0] s0
    );

assign s0 = ({x[6:0], x[31:7]} ^ {x[17:0], x[31:18]} ^ (x >> 3));

endmodule


// σ₁(x)
module sha256_s1 (
    input wire [31:0] x,
    output wire [31:0] s1
    );

assign s1 = ({x[16:0], x[31:17]} ^ {x[18:0], x[31:19]} ^ (x >> 10));

endmodule


// a machine that delivers round constants
module sha256_K_machine (
    input clk,
    input rst,
    output [31:0] K
    );

  reg [2047:0] rom_c;// 64 *32
wire [2047:0] rom_e = { rom_c[2015:0], rom_c[2047:2016] };
assign K = rom_c[2047:2016];

always @(posedge clk)
begin
  if (rst) begin // first time the sha256 start  
        rom_c <= {
            32'h428a2f98, 32'h71374491, 32'hb5c0fbcf, 32'he9b5dba5,
            32'h3956c25b, 32'h59f111f1, 32'h923f82a4, 32'hab1c5ed5,
            32'hd807aa98, 32'h12835b01, 32'h243185be, 32'h550c7dc3,
            32'h72be5d74, 32'h80deb1fe, 32'h9bdc06a7, 32'hc19bf174,
            32'he49b69c1, 32'hefbe4786, 32'h0fc19dc6, 32'h240ca1cc,
            32'h2de92c6f, 32'h4a7484aa, 32'h5cb0a9dc, 32'h76f988da,
            32'h983e5152, 32'ha831c66d, 32'hb00327c8, 32'hbf597fc7,
            32'hc6e00bf3, 32'hd5a79147, 32'h06ca6351, 32'h14292967,
            32'h27b70a85, 32'h2e1b2138, 32'h4d2c6dfc, 32'h53380d13,
            32'h650a7354, 32'h766a0abb, 32'h81c2c92e, 32'h92722c85,
            32'ha2bfe8a1, 32'ha81a664b, 32'hc24b8b70, 32'hc76c51a3,
            32'hd192e819, 32'hd6990624, 32'hf40e3585, 32'h106aa070,
            32'h19a4c116, 32'h1e376c08, 32'h2748774c, 32'h34b0bcb5,
            32'h391c0cb3, 32'h4ed8aa4a, 32'h5b9cca4f, 32'h682e6ff3,
            32'h748f82ee, 32'h78a5636f, 32'h84c87814, 32'h8cc70208,
            32'h90befffa, 32'ha4506ceb, 32'hbef9a3f7, 32'hc67178f2
        };
    end else begin
        rom_c <= rom_e;
    end
end

endmodule


// initial hash values
module sha256_H_i(
  output [255:0] H_i
    );

assign H_i = {
    32'h6A09E667, 32'hBB67AE85, 32'h3C6EF372, 32'hA54FF53A,
    32'h510E527F, 32'h9B05688C, 32'h1F83D9AB, 32'h5BE0CD19
};

endmodule


//round computation the values
module sha2_round #(
    parameter WORDSIZE=0
) (
    input [WORDSIZE-1:0] Kj, Wj,
    input [WORDSIZE-1:0] a_in, b_in, c_in, d_in, e_in, f_in, g_in, h_in,
    input [WORDSIZE-1:0] Ch_e_f_g, Maj_a_b_c, S0_a, S1_e,
    output [WORDSIZE-1:0] a_out, b_out, c_out, d_out, e_out, f_out, g_out, h_out
    );

wire [WORDSIZE-1:0] T1 = h_in + S1_e + Ch_e_f_g + Kj + Wj;
wire [WORDSIZE-1:0] T2 = S0_a + Maj_a_b_c;

assign a_out = T1 + T2;
assign b_out = a_in;
assign c_out = b_in;
assign d_out = c_in;
assign e_out = d_in + T1;
assign f_out = e_in;
assign g_out = f_in;
assign h_out = g_in;

endmodule
//round computation end

// Ch(x,y,z)
module Ch #(parameter WORDSIZE=0) (
    input wire [WORDSIZE-1:0] x, y, z,
    output wire [WORDSIZE-1:0] Ch
    );

assign Ch = ((x & y) ^ (~x & z));

endmodule


// Maj(x,y,z)
module Maj #(parameter WORDSIZE=0) (
    input wire [WORDSIZE-1:0] x, y, z,
    output wire [WORDSIZE-1:0] Maj
    );

assign Maj = (x & y) ^ (x & z) ^ (y & z);

endmodule


// the message schedule: a machine that generates Wt values
module W_machine #(parameter WORDSIZE=1) (
    input clk,
    input [WORDSIZE*16-1:0] M,
    input M_valid,
    output [WORDSIZE-1:0] W_tm2, W_tm15,
    input [WORDSIZE-1:0] s1_Wtm2, s0_Wtm15,
    output [WORDSIZE-1:0] W
    );

// W(t-n) values, from the perspective of Wt_next
assign W_tm2 = W_stack_c[WORDSIZE*2-1:WORDSIZE*1];
assign W_tm15 = W_stack_c[WORDSIZE*15-1:WORDSIZE*14];
wire [WORDSIZE-1:0] W_tm7 = W_stack_c[WORDSIZE*7-1:WORDSIZE*6];
wire [WORDSIZE-1:0] W_tm16 = W_stack_c[WORDSIZE*16-1:WORDSIZE*15];
// Wt_next is the next Wt to be pushed to the queue, will be consumed in 16 rounds
wire [WORDSIZE-1:0] Wt_next = s1_Wtm2 + W_tm7 + s0_Wtm15 + W_tm16;

reg [WORDSIZE*16-1:0] W_stack_c;
wire [WORDSIZE*16-1:0] W_stack_e = {W_stack_c[WORDSIZE*15-1:0], Wt_next};
assign W = W_stack_c[WORDSIZE*16-1:WORDSIZE*15];

always @(posedge clk)
begin
    if (M_valid) begin
        W_stack_c <= M;
    end else begin
        W_stack_c <= W_stack_e;
    end
end

endmodule