`timescale 1ns / 1ps

module adder (
  input a,
  input b,
  input cin,
  output s,
  output cout
  );

//  assign {s,cout} = a + b + cin;
assign s = (~a & ~b & cin) | (~a & b & ~cin) | (a & ~b & ~cin) | (a & b & cin);
assign cout = (a & b) | (a & cin) | (b & cin);

endmodule // adder


module wallace_tree (
  input [13:0] Cin,
  input [16:0] Pin,
  output [13:0] Cout,
  output S,
  output C
  );

  wire s1;
  wire s2;
  wire s3;
  wire s4;
  wire s5;
  wire s6;
  wire s7;
  wire s8;
  wire s9;
  wire s10;
  wire s11;
  wire s12;
  wire s13;
  wire s14;

  adder u_adder1  (.a(Pin[4]),  .b(Pin[3]),  .cin(Pin[2]),  .s(s1),  .cout(Cout[4]));
  adder u_adder2  (.a(Pin[7]),  .b(Pin[6]),  .cin(Pin[5]),  .s(s2),  .cout(Cout[3]));
  adder u_adder3  (.a(Pin[10]), .b(Pin[9]),  .cin(Pin[8]),  .s(s3),  .cout(Cout[2]));
  adder u_adder4  (.a(Pin[13]), .b(Pin[12]), .cin(Pin[11]), .s(s4),  .cout(Cout[1]));
  adder u_adder5  (.a(Pin[16]), .b(Pin[15]), .cin(Pin[14]), .s(s5),  .cout(Cout[0]));
  adder u_adder6  (.a(Cin[2]),  .b(Cin[3]),  .cin(Cin[4]),  .s(s6),  .cout(Cout[8]));
  adder u_adder7  (.a(Pin[0]),  .b(Cin[0]),  .cin(Cin[1]),  .s(s7),  .cout(Cout[7]));
  adder u_adder8  (.a(s2),      .b(s1),      .cin(Pin[1]),  .s(s8),  .cout(Cout[6]));
  adder u_adder9  (.a(s5),      .b(s4),      .cin(s3),      .s(s9),  .cout(Cout[5]));
  adder u_adder10 (.a(s6),      .b(Cin[5]),  .cin(Cin[6]),  .s(s10), .cout(Cout[10]));
  adder u_adder11 (.a(s9),      .b(s8),      .cin(s7),      .s(s11), .cout(Cout[9]));
  adder u_adder12 (.a(Cin[8]),  .b(Cin[9]),  .cin(Cin[10]), .s(s12), .cout(Cout[12]));
  adder u_adder13 (.a(s11),     .b(s10),     .cin(Cin[7]),  .s(s13), .cout(Cout[11]));
  adder u_adder14 (.a(s13),     .b(s12),     .cin(Cin[11]), .s(s14), .cout(Cout[13]));
  adder u_adder15 (.a(s14),     .b(Cin[12]), .cin(Cin[13]), .s(S),   .cout(C));

endmodule // wallace_tree
