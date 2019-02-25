`timescale 10 ns / 1 ns

`define DATA_WIDTH 32
`define ADDR_WIDTH 5

module reg_file(
	input clk,
	input resetn,
	input [`ADDR_WIDTH - 1:0] waddr,
	input [`ADDR_WIDTH - 1:0] raddr1,
	input [`ADDR_WIDTH - 1:0] raddr2,
	input wen,
	input [2:0] Wreg_strb,
	input [`DATA_WIDTH - 1:0] wdata,
	output [`DATA_WIDTH - 1:0] rdata1,
	output [`DATA_WIDTH - 1:0] rdata2
);

	// TODO: Please add your logic code here
reg [31:0] reg_pile [0:31];
integer i;

always@(posedge clk)
begin
	if(!resetn)
	begin
	for(i = 0; i < 32; i = i + 1)
		reg_pile[i] <= 32'd0;
	end
	else
	begin
		if(wen)
		begin
			case(Wreg_strb)   //Wreg_strb[2]==0 then left
			3'b000:  reg_pile[waddr][31:24] <= wdata[7:0];
			3'b001:  reg_pile[waddr][31:16] <= wdata[15:0];
			3'b010:  reg_pile[waddr][31:8]  <= wdata[23:0];
			3'b011:  reg_pile[waddr]        <= wdata;
			3'b100:  reg_pile[waddr][7:0]   <= wdata[7:0];
			3'b101:  reg_pile[waddr][15:0]  <= wdata[15:0];
			3'b110:  reg_pile[waddr][23:0]  <= wdata[23:0];
			3'b111:  reg_pile[waddr]        <= wdata;
			default: reg_pile[waddr]        <= wdata;
			endcase
		end
	end
end

assign rdata1 = (raddr1==5'd0) ? 32'd0 : reg_pile[raddr1];
assign rdata2 = (raddr2==5'd0) ? 32'd0 : reg_pile[raddr2];
endmodule
