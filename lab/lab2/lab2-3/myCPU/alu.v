`timescale 10 ns / 1 ns

`define DATA_WIDTH 32

module alu(
	input  [`DATA_WIDTH - 1:0] A,
	input  [`DATA_WIDTH - 1:0] B,
	input  [3:0] ALUci,
	output Overflow,
	output CarryOut,
	output Zero,
	output [`DATA_WIDTH - 1:0] Result
);

	// TODO: Please add your logic code here
parameter AND  = 4'b0000,
          OR   = 4'b0001,
		      NOR  = 4'b1001,
		      XOR  = 4'b1010,
		      NOPA = 4'b1100,
		      ADD  = 4'b0010,
	    	  SUB  = 4'b0110,
		      SLT  = 4'b0111,
		      SLTU = 4'b0100,
		      SL   = 4'b0101,
		      SRL  = 4'b0011,
		      SRA  = 4'b1011;

wire [`DATA_WIDTH - 1:0] C; wire [`DATA_WIDTH - 1:0] D; wire Cout; wire Cin;

wire alu_and; wire alu_or;   wire alu_nor; wire alu_xor; wire alu_nopa; wire alu_add; wire alu_sub;
wire alu_slt; wire alu_sltu; wire alu_sl;  wire alu_srl; wire alu_sra;

wire [31:0] alu_and_result;
wire [31:0] alu_or_result;
wire [31:0] alu_nor_result;
wire [31:0] alu_xor_result;
wire [31:0] alu_nopa_result;
wire [31:0] alu_add_result;
wire [31:0] alu_sub_result;
wire [31:0] alu_slt_result;
wire [31:0] alu_sltu_result;
wire [31:0] alu_sl_result;
wire [31:0] alu_srl_result;
wire [31:0] alu_sra_result;

assign alu_and  = (ALUci==AND);
assign alu_or   = (ALUci==OR);
assign alu_nor  = (ALUci==NOR);
assign alu_xor  = (ALUci==XOR);
assign alu_nopa = (ALUci==NOPA);
assign alu_add  = (ALUci==ADD);
assign alu_sub  = (ALUci==SUB);
assign alu_slt  = (ALUci==SLT);
assign alu_sltu = (ALUci==SLTU);
assign alu_sl   = (ALUci==SL);
assign alu_srl  = (ALUci==SRL);
assign alu_sra  = (ALUci==SRA);

assign {Cout,C}        = A + D + Cin;
assign alu_and_result  = A & B;
assign alu_or_result   = A | B;
assign alu_nor_result  = ~alu_or_result;
assign alu_xor_result  = A ^ B;
assign alu_nopa_result = A;
assign alu_add_result  = C;
assign alu_sub_result  = C;
assign alu_slt_result  = {31'd0,Overflow ^ C[31]};
assign alu_sltu_result = {31'd0,CarryOut};
assign alu_sl_result   = (B << A[4:0]);
assign alu_srl_result  = (B >> A[4:0]);
assign alu_sra_result  = ({{31{B[31]}},B} >> A[4:0]);

assign Overflow = (((~A[31] & ~B[31] & C[31])  | (A[31]  & B[31] & ~C[31])) & alu_add)
                | (((A[31]  & ~B[31] & ~C[31]) | (~A[31] & B[31] & C[31]))  & alu_sub)
                | (((A[31]  & ~B[31] & ~C[31]) | (~A[31] & B[31] & C[31]))  & alu_slt);

assign CarryOut = (alu_add & Cout) | ((alu_sub | alu_sltu) & ~Cout);
assign Cin      = ~alu_add & (alu_sub | alu_slt | alu_sltu);
assign D        = (alu_sub | alu_slt | alu_sltu) ? ~B : B;

assign Result   = ({32{alu_and}}  & alu_and_result)  |
                  ({32{alu_or}}   & alu_or_result)   |
                  ({32{alu_nor}}  & alu_nor_result)  |
                  ({32{alu_xor}}  & alu_xor_result)  |
                  ({32{alu_nopa}} & alu_nopa_result) |
                  ({32{alu_add}}  & alu_add_result)  |
                  ({32{alu_sub}}  & alu_sub_result)  |
                  ({32{alu_slt}}  & alu_slt_result)  |
                  ({32{alu_sltu}} & alu_sltu_result) |
                  ({32{alu_sl}}   & alu_sl_result)   |
                  ({32{alu_srl}}  & alu_srl_result)  |
                  ({32{alu_sra}}  & alu_sra_result);

assign Zero     = (Result == 32'd0) ? 1 : 0;

endmodule
