`timescale 1ns / 1ps

module booth (
  input [2:0] y,
  input [63:0] x,
  output [63:0] p,
  output c
  );

  wire s_1px;
  wire s_1nx;
  wire s_2px;
  wire s_2nx;

  assign s_1nx = (y[2] & y[1] & ~y[0]) | (y[2] & ~y[1] & y[0]);
  assign s_1px = (~y[2] & y[1] & ~y[0]) | (~y[2] & ~y[1] & y[0]);
  assign s_2nx = (y[2] & ~y[1] & ~y[0]);
  assign s_2px = (~y[2] & y[1] & y[0]);
  assign c = s_1nx | s_2nx;

  wire [63:0] x_sll;

  assign x_sll = {x[62:0],1'b0};

  genvar i;

  generate
    for(i = 0; i < 64; i = i + 1)
    begin
      assign p[i] = (s_1nx & ~x[i]) | (s_2nx & ~x_sll[i]) | (s_1px & x[i]) | (s_2px & x_sll[i]);
    end
  endgenerate

endmodule // booth
