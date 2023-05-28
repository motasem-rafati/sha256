module testbed;

reg input_valid = 0;

wire output_valid_256;
  wire [255:0] H_i_256, H_out_256;
  
  sha256_H_i sha256_H_i (.H_i(H_i_256));// initial hash values
  
sha256_block sha256_block (
    .clk(clk),
  .H_in(H_i_256), .M_in(M_sha256_PSUT),
    .input_valid(input_valid),
    .H_out(H_out_256),
    .output_valid(output_valid_256)
);



// the "abc" test vector, padded abc as hex in 616263 so the 8 in for 1000 so you know where the 0's start and 18 is hex number of how bit abc is
  wire [511:0] M_sha256_PSUT = {
  256'h5053555480000000000000000000000000000000000000000000000000000000,
  256'h0000000000000000000000000000000000000000000000000000000000000020
};







// driver

reg [31:0] ticks = 0;
reg clk = 1'b0;


initial begin
  $display("starting");
  loop;
  input_valid = 1'b1;
  loop;
  input_valid = 1'b0;
  repeat (64) begin
    loop;
  end
  $display("done");
  $finish;
end

task loop;
begin
  #1;
  
  clk = 1;
  #1;
  clk = 0;
  dumpstate;
end
endtask

task dumpstate;
begin
  
  $display("%b %h", output_valid_256, H_out_256);
$dumpfile("dump.vcd"); $dumpvars;

end
endtask

endmodule