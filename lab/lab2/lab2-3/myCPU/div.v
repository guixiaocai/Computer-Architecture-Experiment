`timescale 10 ns / 1 ns

module div (
  input div_clk,
  input resetn,
  input div,
  input div_signed,
  input [31:0] x,
  input [31:0] y,
  output [31:0]s,
  output [31:0] r,
  output complete
  );

  wire s_sign;
  wire r_sign;
  wire [31:0] x_abs;
  wire [31:0] y_abs;

  assign s_sign = div_signed ? (x[31] ^ y[31]) : 1'b0;
  assign r_sign = div_signed ? x[31] : 1'b0;
  assign x_abs  = div_signed ? (x[31] ? ~x + 32'd1 : x) : x;
  assign y_abs  = div_signed ? (y[31] ? ~y + 32'd1 : y) : y;

  wire [63:0] op_x_tmp;
  wire [32:0] op_x_high;
  wire        op_x_update;
  wire [32:0] sub_res;
  wire [31:0] r_tmp;
  wire [31:0] s_tmp1;
  reg  [63:0] op_x;
  reg  [32:0] op_y;
  reg  [5:0]  count;
  reg  [31:0] s_tmp;
  reg         complete;

  always @ ( posedge div_clk )
  begin
    if(!resetn)
    begin
      complete <= 1'b0;
      count    <= 6'd0;
      s_tmp    <= 32'd0;
    end
    else if(div&&!complete)
    begin
      if(count == 6'd0)
      begin
        op_x <= {32'd0,x_abs};
        op_y <= {1'b0,y_abs};
      end
      else if(op_x_update)
      begin
        op_x <= {sub_res,op_x_tmp[30:0]};
      end
      else if(!op_x_update)
        op_x <= op_x_tmp;
      count <= count == 6'd32 ? 6'd0 : count + 6'd1;
      complete <= count == 6'd32 ? 1'b1 : complete;
      s_tmp <= {s_tmp1[31:1],op_x_update};
    end
    else if(complete)
      complete <= 1'b0;
  end

assign s_tmp1 = (count == 6'd0) ? s_tmp : s_tmp << 1;
assign op_x_tmp = (count == 6'd0 || count == 6'd1) ? op_x : op_x << 1;
assign op_x_high = op_x_tmp[63:31];
assign sub_res = op_x_high + ~op_y + 1;
assign op_x_update = ~sub_res[32];
assign r_tmp = complete ? op_x[62:31] : 32'd0;
assign s = div_signed ? (s_sign ? ~s_tmp + 32'd1 : s_tmp) : s_tmp;
assign r = div_signed ? (r_sign ? ~r_tmp + 32'd1 : r_tmp) : r_tmp;

endmodule // div
