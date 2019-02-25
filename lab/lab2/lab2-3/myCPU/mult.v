`timescale 10 ns / 1 ns

module mul(
  input mul_clk,
  input resetn,
  input mul_signed,
  input [31:0] x,
  input [31:0] y,
  output [63:0] result
  );

  wire [63:0] x_ext;
  wire [33:0] y_ext;

  assign x_ext = mul_signed ? {{32{x[31]}},x} : {32'd0,x};
  assign y_ext = mul_signed ? {{2{y[31]}},y} : {2'b0,y};

  wire [63:0] p1;  wire [63:0] p2;  wire [63:0] p3;  wire [63:0] p4;  wire [63:0] p5;  wire [63:0] p6;  wire [63:0] p7;
  wire [63:0] p8;  wire [63:0] p9;  wire [63:0] p10; wire [63:0] p11; wire [63:0] p12; wire [63:0] p13; wire [63:0] p14;
  wire [63:0] p15; wire [63:0] p16; wire [63:0] p17; wire c1;         wire c2;         wire c3;         wire c4;
  wire c5;         wire c6;         wire c7;         wire c8;         wire c9;         wire c10;        wire c11;
  wire c12;        wire c13;        wire c14;        wire c15;        wire c16;        wire c17;

  booth u_booth1  (.y({y_ext[1:0],1'b0}), .x(x_ext),               .p(p1),  .c(c1));
  booth u_booth2  (.y(y_ext[3:1]),        .x({x_ext[61:0],2'd0}),  .p(p2),  .c(c2));
  booth u_booth3  (.y(y_ext[5:3]),        .x({x_ext[59:0],4'd0}),  .p(p3),  .c(c3));
  booth u_booth4  (.y(y_ext[7:5]),        .x({x_ext[57:0],6'd0}),  .p(p4),  .c(c4));
  booth u_booth5  (.y(y_ext[9:7]),        .x({x_ext[55:0],8'd0}),  .p(p5),  .c(c5));
  booth u_booth6  (.y(y_ext[11:9]),       .x({x_ext[53:0],10'd0}), .p(p6),  .c(c6));
  booth u_booth7  (.y(y_ext[13:11]),      .x({x_ext[51:0],12'd0}), .p(p7),  .c(c7));
  booth u_booth8  (.y(y_ext[15:13]),      .x({x_ext[49:0],14'd0}), .p(p8),  .c(c8));
  booth u_booth9  (.y(y_ext[17:15]),      .x({x_ext[47:0],16'd0}), .p(p9),  .c(c9));
  booth u_booth10 (.y(y_ext[19:17]),      .x({x_ext[45:0],18'd0}), .p(p10), .c(c10));
  booth u_booth11 (.y(y_ext[21:19]),      .x({x_ext[43:0],20'd0}), .p(p11), .c(c11));
  booth u_booth12 (.y(y_ext[23:21]),      .x({x_ext[41:0],22'd0}), .p(p12), .c(c12));
  booth u_booth13 (.y(y_ext[25:23]),      .x({x_ext[39:0],24'd0}), .p(p13), .c(c13));
  booth u_booth14 (.y(y_ext[27:25]),      .x({x_ext[37:0],26'd0}), .p(p14), .c(c14));
  booth u_booth15 (.y(y_ext[29:27]),      .x({x_ext[35:0],28'd0}), .p(p15), .c(c15));
  booth u_booth16 (.y(y_ext[31:29]),      .x({x_ext[33:0],30'd0}), .p(p16), .c(c16));
  booth u_booth17 (.y(y_ext[33:31]),      .x({x_ext[31:0],32'd0}), .p(p17), .c(c17));

  reg [63:0] p1_tmp;  reg [63:0] p2_tmp;  reg [63:0] p3_tmp;  reg [63:0] p4_tmp;  reg [63:0] p5_tmp;  reg [63:0] p6_tmp;  reg [63:0] p7_tmp;
  reg [63:0] p8_tmp;  reg [63:0] p9_tmp;  reg [63:0] p10_tmp; reg [63:0] p11_tmp; reg [63:0] p12_tmp; reg [63:0] p13_tmp; reg [63:0] p14_tmp;
  reg [63:0] p15_tmp; reg [63:0] p16_tmp; reg [63:0] p17_tmp; reg c1_tmp;         reg c2_tmp;         reg c3_tmp;         reg c4_tmp;
  reg c5_tmp;         reg c6_tmp;         reg c7_tmp;         reg c8_tmp;         reg c9_tmp;         reg c10_tmp;        reg c11_tmp;
  reg c12_tmp;        reg c13_tmp;        reg c14_tmp;        reg c15_tmp;        reg c16_tmp;        reg c17_tmp;

  always @(posedge mul_clk)
  begin
    if(!resetn)
    begin
      p1_tmp  <= 64'd0; p2_tmp  <= 64'd0; p3_tmp  <= 64'd0; p4_tmp  <= 64'd0; p5_tmp  <= 64'd0; p6_tmp  <= 64'd0; p7_tmp  <= 64'd0;
      p8_tmp  <= 64'd0; p9_tmp  <= 64'd0; p10_tmp <= 64'd0; p11_tmp <= 64'd0; p12_tmp <= 64'd0; p13_tmp <= 64'd0; p14_tmp <= 64'd0;
      p15_tmp <= 64'd0; p16_tmp <= 64'd0; p17_tmp <= 64'd0; c1_tmp  <= 1'b0;  c2_tmp  <= 1'b0;  c3_tmp  <= 1'b0;  c4_tmp  <= 1'b0;
      c5_tmp  <= 1'b0;  c6_tmp  <= 1'b0;  c7_tmp  <= 1'b0;  c8_tmp  <= 1'b0;  c9_tmp  <= 1'b0;  c10_tmp <= 1'b0;  c11_tmp <= 1'b0;
      c12_tmp <= 1'b0;  c13_tmp <= 1'b0;  c14_tmp <= 1'b0;  c15_tmp <= 1'b0;  c16_tmp <= 1'b0;  c17_tmp <= 1'b0;
    end
    else
    begin
      p1_tmp  <= p1;  p2_tmp  <= p2;  p3_tmp  <= p3;  p4_tmp  <= p4;  p5_tmp  <= p5;  p6_tmp  <= p6;  p7_tmp  <= p7;
      p8_tmp  <= p8;  p9_tmp  <= p9;  p10_tmp <= p10; p11_tmp <= p11; p12_tmp <= p12; p13_tmp <= p13; p14_tmp <= p14;
      p15_tmp <= p15; p16_tmp <= p16; p17_tmp <= p17; c1_tmp  <= c1;  c2_tmp  <= c2;  c3_tmp  <= c3;  c4_tmp  <= c4;
      c5_tmp  <= c5;  c6_tmp  <= c6;  c7_tmp  <= c7;  c8_tmp  <= c8;  c9_tmp  <= c9;  c10_tmp <= c10; c11_tmp <= c11;
      c12_tmp <= c12; c13_tmp <= c13; c14_tmp <= c14; c15_tmp <= c15; c16_tmp <= c16; c17_tmp <= c17;
    end
  end

  wire [16:0] Pin_com [63:0];

  genvar i;

  generate
    for(i = 0; i < 64; i = i + 1)
    begin
      assign Pin_com[i] = {p17_tmp[i],p16_tmp[i],p15_tmp[i],p14_tmp[i],p13_tmp[i],p12_tmp[i],p11_tmp[i],p10_tmp[i],
                     p9_tmp[i],p8_tmp[i],p7_tmp[i],p6_tmp[i],p5_tmp[i],p4_tmp[i],p3_tmp[i],p2_tmp[i],p1_tmp[i]};
    end
  endgenerate

  wire [13:0] Cin;
  assign Cin = {c1_tmp,c2_tmp,c3_tmp,c4_tmp,c5_tmp,c6_tmp,c7_tmp,c8_tmp,c9_tmp,c10_tmp,c11_tmp,c12_tmp,c13_tmp,c14_tmp};

  wire [13:0] Cout [64:0];
  wire [63:0] S;
  wire [63:0] C;

  assign Cout[0] = Cin;
  generate
    for(i = 0; i < 64; i = i + 2)
    begin : label
      wallace_tree u_wallace_tree1(.Cin(Cout[i]), .Pin(Pin_com[i]), .Cout(Cout[i+1]), .S(S[i]), .C(C[i]));
      wallace_tree u_wallace_tree2(.Cin(Cout[i+1]), .Pin(Pin_com[i+1]), .Cout(Cout[i+2]), .S(S[i+1]), .C(C[i+1]));
    end
  endgenerate

  assign result = S + {C[62:0],c15_tmp} + c16_tmp;

endmodule // mult
