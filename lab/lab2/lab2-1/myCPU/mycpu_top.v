`timescale 10ns / 1ns

`define addiu 6'b001001
`define lw    6'b100011
`define sw    6'b101011
`define bne   6'b000101
`define beq   6'b000100
`define j     6'b000010
`define jal   6'b000011
`define jr    6'b001000
`define jalr  6'b001001
`define lui   6'b001111
`define slt   6'b101010
`define sltu  6'b101011
`define slti  6'b001010
`define sltiu 6'b001011
`define subu  6'b100011
`define addu  6'b100001
`define and   6'b100100
`define andi  6'b001100
`define nor   6'b100111
`define xor   6'b100110
`define xori  6'b001110
`define or    6'b100101
`define move  6'b100101
`define ori   6'b001101
`define sll   6'b000000
`define nop   6'b000000
`define sllv  6'b000100
`define sra   6'b000011
`define srav  6'b000111
`define srl   6'b000010
`define srlv  6'b000110
`define movz  6'b001010
`define movn  6'b001011
`define blez  6'b000110
`define bgez  6'b000001
`define bltz  6'b000001
`define lwl   6'b100010
`define lwr   6'b100110
`define lb    6'b100000
`define lh    6'b100001
`define lbu   6'b100100
`define lhu   6'b100101
`define swl   6'b101010
`define swr   6'b101110
`define sb    6'b101000
`define sh    6'b101001
`define bgtz  6'b000111


module mycpu_top(
	input  resetn,
	input  clk,

	//Instruction_Sram
	output        inst_sram_en,
	output [3:0]  inst_sram_wen,
	input  [31:0] inst_sram_rdata,
	output [31:0] inst_sram_addr,
	output [31:0] inst_sram_wdata,

	//Data_Sram
	output [3:0]  data_sram_wen,
	output        data_sram_en,
	output [31:0] data_sram_wdata,
	output [31:0] data_sram_addr,
	input  [31:0] data_sram_rdata,

	//Debug_Signal
	output [31:0] debug_wb_pc,
	output [3:0]  debug_wb_rf_wen,
	output [4:0]  debug_wb_rf_wnum,
	output [31:0] debug_wb_rf_wdata
);

  //TODO: Please add your MIPS CPU code here
wire [4:0]  raddr1;
wire [4:0]  raddr2;
wire [31:0] A;
wire [31:0] B;
reg  [3:0] ALUci_ex;
wire Overflow;
wire CarryOut;
wire Zero;
wire [31:0] Result;
wire [4:0] waddr;
wire RegWrite;
wire [31:0] wdata;
wire [31:0] rdata1;
wire [31:0] rdata2;

alu u_alu(.A(A),.B(B),.ALUci(ALUci_ex),.Overflow(Overflow),.CarryOut(CarryOut),.Zero(Zero),.Result(Result));

reg_file u_reg_file(.clk(clk),.resetn(resetn),.waddr(waddr),.raddr1(raddr1),.raddr2(raddr2),.wen(RegWrite),.wdata(wdata),.rdata1(rdata1),.rdata2(rdata2));

wire [5:0]  opcode;
wire [4:0]  rs;
wire [4:0]  rt;
wire [4:0]  rd;
wire [5:0]  func;
wire [4:0]  sa;
wire [15:0] offset;
wire [25:0] inst_index;
wire [31:0] IR;
reg  [31:0] inst_sram_addr;
reg  inst_jump_ex;
wire inst_jump;
wire        pipe1_allowin;
wire [31:0] next_PC;

assign inst_sram_wen   = 4'd0;
assign inst_sram_wdata = 32'd0;
assign opcode          = IR[31:26];
assign rs              = IR[25:21];
assign rt              = IR[20:16];
assign rd              = IR[15:11];
assign func            = IR[5:0];
assign sa              = IR[10:6];
assign offset          = IR[15:0];
assign inst_index      = IR[25:0];

//PC
always@(posedge clk)
begin
	if(!resetn)
		inst_sram_addr <= 32'hbfc00000;
	else if(inst_sram_en | inst_jump_ex)
	begin
		inst_sram_addr <= inst_jump_ex ? next_PC : inst_sram_addr+32'd4;
	end
end

//decode
wire inst_lui;
wire inst_addu;
wire inst_addiu;
wire inst_subu;
wire inst_slt;
wire inst_sltu;
wire inst_and;
wire inst_or;
wire inst_xor;
wire inst_nor;
wire inst_sll;
wire inst_srl;
wire inst_sra;
wire inst_lw;
wire inst_sw;
wire inst_beq;
wire inst_bne;
wire inst_jal;
wire inst_jr;
wire bran_id;

wire [1:0] ALUsrcb_id;
wire [1:0] ALUsrca_id;
wire [1:0] waddr_src_id;
wire [1:0] wdata_src_id;
wire       RegWrite_id;
wire       data_sram_en_id;
wire [1:0] PCsrc_id;
wire [3:0] ALUci_id;

assign inst_lui   = (opcode==`lui);
assign inst_addu  = (opcode==6'd0) && (func==`addu);
assign inst_addiu = (opcode==`addiu);
assign inst_subu  = (opcode==6'd0) && (func==`subu);
assign inst_slt   = (opcode==6'd0) && (func==`slt);
assign inst_sltu  = (opcode==6'd0) && (func==`sltu);
assign inst_and   = (opcode==6'd0) && (func==`and);
assign inst_or    = (opcode==6'd0) && (func==`or);
assign inst_xor   = (opcode==6'd0) && (func==`xor);
assign inst_nor   = (opcode==6'd0) && (func==`nor);
assign inst_sll   = (opcode==6'd0) && (func==`sll);
assign inst_srl   = (opcode==6'd0) && (func==`srl);
assign inst_sra   = (opcode==6'd0) && (func==`sra);
assign inst_lw    = (opcode==`lw);
assign inst_sw    = (opcode==`sw);
assign inst_beq   = (opcode==`beq);
assign inst_bne   = (opcode==`bne);
assign inst_jal   = (opcode==`jal);
assign inst_jr    = (opcode==6'd0) && (func==`jr);
assign inst_jump  = inst_beq | inst_bne | inst_jal | inst_jr;


assign bran_id         = inst_beq | inst_bne;
assign ALUsrcb_id      = inst_lui | inst_addiu | inst_lw | inst_sw;
assign ALUsrca_id      = inst_sll | inst_srl | inst_sra;
assign waddr_src_id[0] = inst_lui | inst_addiu | inst_lw;
assign waddr_src_id[1] = inst_jal;
assign wdata_src_id[0] = inst_lw | inst_jal;
assign wdata_src_id[1] = inst_lui | inst_jal;
assign RegWrite_id     = ~inst_sw & ~inst_beq & ~inst_bne & ~inst_jr;
assign data_sram_en_id = inst_lw | inst_sw;
assign PCsrc_id[0]     = inst_jr;
assign PCsrc_id[1]     = inst_jal;
assign ALUci_id[0]     = inst_slt | inst_or | inst_nor | inst_sll | inst_srl | inst_sra;
assign ALUci_id[1]     = inst_lui | inst_addu | inst_addiu | inst_subu | inst_slt | inst_xor | inst_srl | inst_sra | inst_lw | inst_sw;
assign ALUci_id[2]     = inst_subu | inst_slt | inst_sltu | inst_sll;
assign ALUci_id[3]     = inst_xor | inst_nor | inst_sra;

//pipeline IF
reg        pipe1_valid;
reg        inst_sram_en;



wire       pipe1_ready_go;
wire       pipe1_to_pipe2_valid;
wire       pipe2_allowin;

assign pipe1_ready_go       = pipe1_valid;
assign pipe1_allowin        = !pipe1_valid || pipe1_ready_go && pipe2_allowin;
assign pipe1_to_pipe2_valid = pipe1_valid && pipe1_ready_go;

always@(posedge clk)
begin
  if(!resetn)
  begin
    pipe1_valid <= 1'b0;
	inst_sram_en <= 1'b0;
  end
  else if(pipe1_allowin && !inst_jump)
  begin
    inst_sram_en <= 1'b1;
	pipe1_valid  <= 1'b1;
  end

  else if(inst_jump)
  begin
	inst_sram_en <= 1'b0;
	pipe1_valid  <= 0;
  end
end

assign IR = inst_sram_rdata;

// pipeline ID
reg  [31:0] PC_id;
reg         pipe2_valid;
reg  [1:0]  PCsrc_ex;
reg  [31:0] PC_ex;
reg         bran_ex;
reg  [31:0] rdata1_ex;
reg  [15:0] offset_ex;
wire [31:0] next_PC_tmp;
wire [31:0] PC_op;
wire        bran_taken;
wire [4:0]  waddr_id;
wire        pipe2_ready_go;
wire        pipe2_to_pipe3_valid;
wire        pipe3_allowin;
wire        taken;

assign pipe2_ready_go       =pipe2_valid;
assign pipe2_allowin        = 1;
assign pipe2_to_pipe3_valid = pipe2_valid && pipe2_ready_go;

always@(posedge clk)
begin
    if(!resetn)
	begin
      pipe2_valid <= 1'b0;
	  PC_id       <= 32'hbfc00000;
    end
    else if(pipe2_allowin)
	begin
      pipe2_valid <= pipe1_to_pipe2_valid;
    end

    if(pipe1_to_pipe2_valid && pipe2_allowin)
	begin
		  PC_id   <= inst_sram_addr;
    end
end

assign raddr1      = rs;
assign raddr2      = rt;
assign waddr_id    = waddr_src_id==2'b01 ? rt : (waddr_src_id==2'b10 ? 5'd31 : rd);

// pipeline EX
reg         inst_beq_ex;
reg         inst_bne_ex;
reg         inst_lw_ex;
reg         inst_sw_ex;
reg  [31:0] rdata2_ex;
reg  [1:0]  ALUsrcb_ex;
reg  [1:0]  ALUsrca_ex;
reg  [4:0]  waddr_ex;
reg  [1:0]  wdata_src_ex;
reg         data_sram_en_ex;
reg  [4:0]  sa_ex;
reg         pipe3_valid;
reg         RegWrite_ex;
reg  [25:0]  inst_index_ex;      
wire        pipe3_ready_go;
wire        pipe3_to_pipe4_valid;
wire        pipe4_allowin;

assign pipe3_ready_go       = pipe3_valid;
assign pipe3_allowin        = 1;
assign pipe3_to_pipe4_valid = pipe3_valid && pipe3_ready_go;

always@(posedge clk)
begin
    if(!resetn)
    begin
      pipe3_valid <= 1'b0;
      inst_jump_ex  <= 1'b0;
    end
    else if(pipe3_allowin)
	begin
      pipe3_valid <= pipe2_to_pipe3_valid;
    end

    if(pipe2_to_pipe3_valid && pipe3_allowin)
		begin
		    inst_beq_ex     <= inst_beq;
		    inst_bne_ex     <= inst_bne;
			inst_lw_ex      <= inst_lw;
			inst_sw_ex      <= inst_sw;
		    rdata1_ex       <= rdata1;
			rdata2_ex       <= rdata2;
			PC_ex           <= PC_id;
			bran_ex         <= bran_id;
			ALUsrcb_ex      <= ALUsrcb_id;
			ALUsrca_ex      <= ALUsrca_id;
			waddr_ex        <= waddr_id;
			wdata_src_ex    <= wdata_src_id;
			RegWrite_ex     <= RegWrite_id;
			data_sram_en_ex <= data_sram_en_id;
			PCsrc_ex        <= PCsrc_id;
			ALUci_ex        <= ALUci_id;
			sa_ex           <= sa;
			offset_ex       <= offset;
			inst_index_ex   <= inst_index;
			inst_jump_ex    <= inst_jump;
    end
end

assign next_PC     = PCsrc_ex==2'b00 ? next_PC_tmp : (PCsrc_ex==2'b10 ? {PC_ex[31:28],inst_index_ex,2'b00} : rdata1_ex);
assign next_PC_tmp = (bran_taken ? PC_ex+32'd4 : PC_ex) + PC_op;
assign PC_op       = bran_taken ? ({{14{offset_ex[15]}},offset_ex,2'd0}) : 32'd4;
assign bran_taken  = bran_ex & taken;
assign taken = bran_ex ? (inst_beq_ex ? rdata1_ex==rdata2_ex : (inst_bne_ex ? rdata1_ex!=rdata2_ex : 1'b0)) : 1'b0;

assign A     = ALUsrca_ex ? sa_ex : rdata1_ex;
assign B     = ALUsrcb_ex ? {{16{offset_ex[15]}},offset_ex} : rdata2_ex;

// pipeline MEM
reg  [31:0] Result_mem;
reg         inst_lw_mem;
reg         inst_sw_mem;
reg  [31:0] rdata2_mem;
reg  [31:0] PC_mem;
reg  [4:0]  waddr_mem;
reg  [1:0]  wdata_src_mem;
reg         data_sram_en_mem;
reg         pipe4_valid;
reg         RegWrite_mem;
wire [31:0] wdata_mem;
wire        pipe4_ready_go;
wire        pipe4_to_pipe5_valid;
wire        pipe5_allowin;

assign pipe4_ready_go       = pipe4_valid;
assign pipe4_allowin        = 1;
assign pipe4_to_pipe5_valid = pipe4_valid && pipe4_ready_go;

always@(posedge clk)
begin
    if(!resetn)
	begin
      pipe4_valid <= 1'b0;
	  data_sram_en_mem <= 1'b0;
    end
    else if(pipe4_allowin)
	begin
      pipe4_valid <= pipe3_to_pipe4_valid;
    end

    if(pipe3_to_pipe4_valid && pipe4_allowin)
	begin
		Result_mem       <= Result;
		inst_lw_mem      <= inst_lw_ex;
	    inst_sw_mem      <= inst_sw_ex;
		rdata2_mem       <= rdata2_ex;
		PC_mem           <= PC_ex;
		waddr_mem        <= waddr_ex;
		wdata_src_mem    <= wdata_src_ex;
		RegWrite_mem     <= RegWrite_ex;
		data_sram_en_mem <= data_sram_en_ex;
    end
end

assign data_sram_addr  = Result_mem;
assign data_sram_wen   = inst_sw_mem ? 4'b1111 : 4'b0000;
assign data_sram_en    = data_sram_en_mem;
assign data_sram_wdata = rdata2_mem;

// pipeline WB
reg  [31:0] PC_wb;
reg  [4:0]  waddr_wb;
reg  [31:0] wdata_wb;
reg         RegWrite_wb;
reg         pipe5_valid;
reg  [1:0]  wdata_src_wb;
reg  [31:0] wdata_from_mem;
reg [31:0]  Result_wb;

assign pipe5_allowin = 1;

always@(posedge clk)
begin
  if(!resetn)
	begin
        pipe5_valid <= 1'b0;
		RegWrite_wb <= 1'b0;
  end
  else if(pipe5_allowin)
  begin
        pipe5_valid <= pipe4_to_pipe5_valid;
  end

  if(pipe4_to_pipe5_valid && pipe5_allowin)
	begin
  	    PC_wb       <= PC_mem;
  	    waddr_wb    <= waddr_mem;
     	wdata_wb    <= wdata_mem;
    	RegWrite_wb <= RegWrite_mem;
		wdata_src_wb <= wdata_src_mem;
		wdata_from_mem <= data_sram_rdata;
		Result_wb      <= Result_mem;
  end
end

assign wdata       = wdata_src_wb==2'b00 ? Result_wb : (wdata_src_wb==2'b10 ? {Result_wb[15:0],16'd0}
                   : (wdata_src_wb==2'b11 ? PC_wb+32'd8 : data_sram_rdata));

assign RegWrite = RegWrite_wb;
assign waddr    = waddr_wb;

//Debug
wire [31:0] debug_wb_pc;
wire [3:0]  debug_wb_rf_wen;
wire [4:0]  debug_wb_rf_wnum;
wire [31:0] debug_wb_rf_wdata;

assign debug_wb_pc       = RegWrite_wb ? PC_wb : debug_wb_pc;
assign debug_wb_rf_wen   = RegWrite_wb ? {4{RegWrite_wb}} : debug_wb_rf_wen;
assign debug_wb_rf_wnum  = RegWrite_wb ? waddr : debug_wb_rf_wnum;
assign debug_wb_rf_wdata = RegWrite_wb ? wdata : debug_wb_rf_wdata;

endmodule
