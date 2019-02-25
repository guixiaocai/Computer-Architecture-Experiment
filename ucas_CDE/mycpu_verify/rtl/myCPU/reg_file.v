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
	input [`DATA_WIDTH - 1:0] wdata,
	output [`DATA_WIDTH - 1:0] rdata1,
	output [`DATA_WIDTH - 1:0] rdata2
);

	// TODO: Please add your logic code here
reg [31:0] reg_pile [0:31];
integer i;

always@(posedge clk)
begin
		if(wen)
		begin
		  reg_pile[waddr] <= wdata;
		end
end

assign rdata1 = (raddr1==5'd0) ? 32'd0 : reg_pile[raddr1];
assign rdata2 = (raddr2==5'd0) ? 32'd0 : reg_pile[raddr2];
endmodule
