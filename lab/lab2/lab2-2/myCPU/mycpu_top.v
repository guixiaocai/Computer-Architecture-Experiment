`timescale 10ns / 1ns

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
reg  [3:0]  ALUci_ex;
wire        Overflow;
wire        CarryOut;
wire        Zero;
wire [31:0] Result;
wire [4:0]  waddr;
wire        RegWrite;
wire [31:0] wdata;
wire [31:0] rdata1;
wire [31:0] rdata2;
/*reg  [31:0] rdata1_ex;
reg  [31:0] rdata2_ex;*/
//wire        mul_signed;
wire [63:0] mul_res;
reg  [31:0] hi;
reg  [31:0] lo;
wire        div_en;
//wire        div_signed;
wire [31:0] div_s;
wire [31:0] div_r;
wire        complete;
reg         inst_mult_ex;
reg         inst_multu_ex;
reg         inst_div_ex;
reg         inst_divu_ex;

alu u_alu(.A(A),.B(B),.ALUci(ALUci_ex),.Overflow(Overflow),.CarryOut(CarryOut),.Zero(Zero),.Result(Result));

reg_file u_reg_file(.clk(clk),.resetn(resetn),.waddr(waddr),.raddr1(raddr1),.raddr2(raddr2),.wen(RegWrite),.wdata(wdata),.rdata1(rdata1),.rdata2(rdata2));

mul u_mul(.mul_clk(clk), .resetn(resetn), .mul_signed(inst_mult_ex), .x(A), .y(B), .result(mul_res));

div u_div(.div_clk(clk), .resetn(resetn), .div(div_en), .div_signed(inst_div_ex), .x(A), .y(B), .s(div_s), .r(div_r), .complete(complete));

wire [5:0]  opcode;
wire [4:0]  rs;
wire [4:0]  rt;
wire [4:0]  rd;
wire [5:0]  func;
wire [4:0]  sa;
wire [15:0] offset;
wire [25:0] inst_index;
wire [31:0] IR;
wire [31:0] IR_tmp;
reg  [31:0] inst_sram_addr;
reg         inst_jump_ex;
wire        inst_jump;
wire        inst_jump_cond;
reg         inst_jump_cond_ex;
wire        pipe1_allowin;
wire [31:0] next_PC;
wire        bran_taken;
reg         mult_done;


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
	else if(inst_sram_en || inst_jump_ex || inst_jump_cond_ex || div_en || inst_mult_ex || inst_multu_ex)
	begin
		inst_sram_addr <= (inst_jump_ex || (inst_jump_cond_ex && bran_taken)) ? next_PC : /*inst_sram_addr + 32'd4;*/
		                  ((inst_jump_cond && !bran_taken) || div_en&&!complete || (inst_mult_ex || inst_multu_ex)&&!mult_done ? inst_sram_addr : inst_sram_addr + 32'd4);
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
wire inst_addi;
wire inst_add;
wire inst_sub;
wire inst_slti;
wire inst_sltiu;
wire inst_andi;
wire inst_ori;
wire inst_xori;
wire inst_sllv;
wire inst_srav;
wire inst_srlv;
wire inst_div;
wire inst_divu;
wire inst_mult;
wire inst_multu;
wire inst_mfhi;
wire inst_mflo;
wire inst_mthi;
wire inst_mtlo;

wire       bran_id;
wire [1:0] ALUsrcb_id;
wire [1:0] ALUsrca_id;
wire [1:0] waddr_src_id;
wire [3:0] wdata_src_id;
wire       RegWrite_id;
wire       data_sram_en_id;
wire [1:0] PCsrc_id;
wire [3:0] ALUci_id;
wire       ALUsrcb_id_unsigned;

assign inst_lui       = (opcode==6'b001111) && (rs==5'b00000);
assign inst_addu      = (opcode==6'b000000) && (sa==5'b00000) && (func==6'b100001);
assign inst_addiu     = (opcode==6'b001001);
assign inst_add       = (opcode==6'b000000) && (sa==5'b00000) && (func==6'b100000);
assign inst_addi      = (opcode==6'b001000);
assign inst_subu      = (opcode==6'b000000) && (sa==5'b00000) && (func==6'b100011);
assign inst_sub       = (opcode==6'b000000) && (sa==5'b00000) && (func==6'b100010);
assign inst_slt       = (opcode==6'b000000) && (sa==5'b00000) && (func==6'b101010);
assign inst_sltu      = (opcode==6'b000000) && (sa==5'b00000) && (func==6'b101011);
assign inst_slti      = (opcode==6'b001010);
assign inst_sltiu     = (opcode==6'b001011);
assign inst_div       = (opcode==6'b000000) && (IR[15:6]==10'd0) && (func==6'b011010);
assign inst_divu      = (opcode==6'b000000) && (IR[15:6]==10'd0) && (func==6'b011011);
assign inst_mult      = (opcode==6'b000000) && (IR[15:6]==10'd0) && (func==6'b011000);
assign inst_multu     = (opcode==6'b000000) && (IR[15:6]==10'd0) && (func==6'b011001);
assign inst_and       = (opcode==6'b000000) && (sa==5'b00000) && (func==6'b100100);
assign inst_andi      = (opcode==6'b001100);
assign inst_or        = (opcode==6'b000000) && (sa==5'b00000) && (func==6'b100101);
assign inst_ori       = (opcode==6'b001101);
assign inst_xor       = (opcode==6'b000000) && (sa==5'b00000) && (func==6'b100110);
assign inst_xori      = (opcode==6'b001110);
assign inst_nor       = (opcode==6'b000000) && (sa==5'b00000) && (func==6'b100111);
assign inst_sll       = (opcode==6'b000000) && (rs==5'b00000) && (func==6'b000000);
assign inst_sllv      = (opcode==6'b000000) && (sa==5'b00000) && (func==6'b000100);
assign inst_srl       = (opcode==6'b000000) && (rs==5'b00000) && (func==6'b000010);
assign inst_srlv      = (opcode==6'b000000) && (sa==5'b00000) && (func==6'b000110);
assign inst_sra       = (opcode==6'b000000) && (rs==5'b00000) && (func==6'b000011);
assign inst_srav      = (opcode==6'b000000) && (sa==5'b00000) && (func==6'b000111);
assign inst_lw        = (opcode==6'b100011);
assign inst_sw        = (opcode==6'b101011);
assign inst_beq       = (opcode==6'b000100);
assign inst_bne       = (opcode==6'b000101);
assign inst_jal       = (opcode==6'b000011);
assign inst_jr        = (opcode==6'b000000) && (IR[20:11]==10'd0) && (sa==5'b00000) && (func==6'b001000);
assign inst_div       = (opcode==6'b000000) && (IR[15:6]==10'd0)  && (func==6'b011010);
assign inst_divu      = (opcode==6'b000000) && (IR[15:6]==10'd0)  && (func==6'b011011);
assign inst_mult      = (opcode==6'b000000) && (IR[15:6]==10'd0)  && (func==6'b011000);
assign inst_multu     = (opcode==6'b000000) && (IR[15:6]==10'd0)  && (func==6'b011001);
assign inst_mfhi      = (opcode==6'b000000) && (IR[25:16]==10'd0) && (sa==5'b00000) && (func==6'b010000);
assign inst_mflo      = (opcode==6'b000000) && (IR[25:16]==10'd0) && (sa==5'b00000) && (func==6'b010010);
assign inst_mthi      = (opcode==6'b000000) && (IR[20:6]==15'd0) && (func==6'b010001);
assign inst_mtlo      = (opcode==6'b000000) && (IR[20:6]==15'd0) && (func==6'b010011);
assign inst_jump      = inst_jal | inst_jr;
assign inst_jump_cond = inst_beq | inst_bne;


assign bran_id         = inst_beq | inst_bne;
assign ALUsrcb_id      = inst_lui | inst_addiu | inst_addi | inst_slti | inst_sltiu | inst_andi | inst_ori | inst_xori | inst_lw | inst_sw;
assign ALUsrca_id      = inst_sll | inst_srl   | inst_sra;
assign waddr_src_id[0] = inst_lui | inst_addiu | inst_addi | inst_slti | inst_sltiu | inst_andi | inst_ori | inst_xori | inst_lw;
assign waddr_src_id[1] = inst_jal;
assign wdata_src_id[0] = inst_lw | inst_jal;
assign wdata_src_id[1] = inst_lui | inst_jal;
assign wdata_src_id[2] = inst_mfhi;
assign wdata_src_id[3] = inst_mflo;
assign RegWrite_id     = ~inst_sw & ~inst_beq & ~inst_bne & ~inst_jr & ~inst_mthi & ~inst_mtlo;
assign data_sram_en_id = inst_lw | inst_sw;
assign PCsrc_id[0]     = inst_jr;
assign PCsrc_id[1]     = inst_jal;
assign ALUci_id[0]     = inst_slt | inst_slti | inst_or | inst_ori |inst_nor | inst_sll | inst_sllv | inst_srl | inst_srlv | inst_sra | inst_srav;
assign ALUci_id[1]     = inst_lui | inst_addu | inst_add | inst_addiu | inst_addi | inst_subu | inst_sub | inst_slt | inst_slti
                       | inst_xor | inst_xori | inst_srl  | inst_srlv | inst_sra | inst_srav | inst_lw    | inst_sw;
assign ALUci_id[2]     = inst_subu | inst_sub | inst_slt | inst_slti | inst_sltu | inst_sltiu | inst_sll | inst_sllv;
assign ALUci_id[3]     = inst_xor | inst_xori | inst_nor | inst_sra | inst_srav;

assign ALUsrcb_id_unsigned = inst_andi | inst_ori | inst_xori;

//pipeline IF
reg        pipe1_valid;
reg        inst_sram_en;
wire       pipe1_ready_go;
wire       pipe1_to_pipe2_valid;
wire       pipe2_allowin;
//reg        mult_done;
reg         pipe2_valid;

assign pipe1_ready_go       = div_en ? complete : ((inst_mult_ex || inst_multu_ex) ? mult_done : !(inst_jump_ex || inst_jump_cond_ex/*pipe1_valid*/));//pipe1_valid;
assign pipe1_allowin        = !pipe1_valid || pipe1_ready_go && pipe2_allowin;
assign pipe1_to_pipe2_valid = pipe1_valid && pipe1_ready_go;

always@(posedge clk)
begin
  if(!resetn)
  begin
    pipe1_valid  <= 1'b0;
	  inst_sram_en <= 1'b0;
  end
  else if(((pipe1_allowin && !(inst_jump || inst_jump_cond) || !pipe1_ready_go))&&!div_en&&!(inst_mult_ex||inst_multu_ex) || complete || mult_done)
  begin
    inst_sram_en <= 1'b1;
	  pipe1_valid  <= 1'b1;
  end

  else if(inst_jump||inst_jump_cond || ((inst_mult_ex || inst_multu_ex)&&!mult_done) || (div_en&&!complete))
  begin
	  inst_sram_en <= 1'b0;
//	  pipe1_valid  <= 1'b0;
  end
	else
	  inst_sram_en <= 1'b0;
end

assign IR_tmp = inst_sram_rdata;
assign IR     = (inst_mult_ex || inst_multu_ex || div_en) && !inst_sram_en ? IR : IR_tmp;

// pipeline ID
reg  [31:0] PC_id;

reg  [1:0]  PCsrc_ex;
reg  [31:0] PC_ex;
reg         bran_ex;
//reg  [31:0] rdata1_ex;
reg  [15:0] offset_ex;
wire [31:0] next_PC_tmp;
wire [31:0] PC_op;
wire [4:0]  waddr_id;
wire        pipe2_ready_go;
wire        pipe2_to_pipe3_valid;
wire        pipe3_allowin;
wire        taken;
//reg  [31:0] inst_sram_rdata_id;

assign pipe2_ready_go       = pipe2_valid;
assign pipe2_allowin        = !pipe2_valid || pipe2_ready_go && pipe3_allowin;//1'b1;
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
//		  inst_sram_rdata_id <= inst_sram_rdata;
    end
end

//assign IR = inst_sram_rdata_id;
assign raddr1      = rs;
assign raddr2      = rt;
assign waddr_id    = waddr_src_id==2'b01 ? rt : (waddr_src_id==2'b10 ? 5'd31 : rd);

// pipeline EX
reg         inst_beq_ex;
reg         inst_bne_ex;
reg         inst_lw_ex;
reg         inst_sw_ex;
reg  [31:0] rdata1_ex;
reg  [31:0] rdata2_ex;
reg  [1:0]  ALUsrcb_ex;
reg  [1:0]  ALUsrca_ex;
reg  [4:0]  waddr_ex;
reg  [3:0]  wdata_src_ex;
reg         data_sram_en_ex;
reg  [4:0]  sa_ex;
reg         pipe3_valid;
reg         RegWrite_ex;
reg  [25:0]  inst_index_ex;
wire        pipe3_ready_go;
wire        pipe3_to_pipe4_valid;
wire        pipe4_allowin;
wire        use_mem_rs;
wire        use_wb_rs;
wire        use_mem_rt;
wire        use_wb_rt;
wire [31:0] rs_data_ex;
wire [31:0] rt_data_ex;
reg  [3:0]  wdata_src_mem;
reg         inst_lw_mem;
reg  [4:0]  waddr_mem;
reg  [4:0]  waddr_wb;
reg  [4:0]  rs_ex;
reg  [4:0]  rt_ex;
reg  [31:0] PC_mem;
reg  [31:0] Result_mem;
reg         ALUsrcb_ex_unsigned;
reg         RegWrite_mem;
reg         RegWrite_wb;
reg  [31:0] wdata_wb;
reg         inst_mthi_ex;
reg         inst_mtlo_ex;

assign pipe3_ready_go       =/* ((rs_ex == waddr_mem) && (rs_ex != 5'd0) && (inst_lw_mem)) || ((rt_ex == waddr_mem) && (rt_ex != 5'd0) && (inst_lw_mem)) ? 1'b0
                            : */div_en ? complete : ((inst_mult_ex || inst_multu_ex) ? mult_done : pipe3_valid);//pipe3_valid;
assign pipe3_allowin        = !pipe3_valid || pipe3_ready_go && pipe4_allowin;//1'b1;
assign pipe3_to_pipe4_valid = pipe3_valid && pipe3_ready_go;

always@(posedge clk)
begin
    if(!resetn)
    begin
      pipe3_valid       <= 1'b0;
      inst_jump_ex      <= 1'b0;
      inst_jump_cond_ex <= 1'b0;
			mult_done         <= 1'b0;
			inst_beq_ex       <= 1'b0;
			inst_bne_ex       <= 1'b0;
			inst_div_ex       <= 1'b0;
			inst_divu_ex      <= 1'b0;
			inst_mult_ex      <= 1'b0;
			inst_multu_ex     <= 1'b0;
    end
    else if(pipe3_allowin)
	  begin
      pipe3_valid <= pipe2_to_pipe3_valid;
    end

    if(pipe2_to_pipe3_valid && pipe3_allowin)
		begin
		  inst_beq_ex       <= inst_beq;
		  inst_bne_ex       <= inst_bne;
			inst_lw_ex        <= inst_lw;
			inst_sw_ex        <= inst_sw;
			inst_mthi_ex      <= inst_mthi;
			inst_mtlo_ex      <= inst_mtlo;
			rs_ex             <= rs;
			rt_ex             <= rt;
		  rdata1_ex         <= rdata1;
			rdata2_ex         <= rdata2;
			PC_ex             <= PC_id;
			bran_ex           <= bran_id;
			ALUsrcb_ex        <= ALUsrcb_id;
			ALUsrca_ex        <= ALUsrca_id;
			waddr_ex          <= waddr_id;
			wdata_src_ex      <= wdata_src_id;
			RegWrite_ex       <= RegWrite_id;
			data_sram_en_ex   <= data_sram_en_id;
			PCsrc_ex          <= PCsrc_id;
			ALUci_ex          <= ALUci_id;
			sa_ex             <= sa;
			offset_ex         <= offset;
			inst_index_ex     <= inst_index;
			inst_jump_ex      <= inst_jump;
			inst_jump_cond_ex <= inst_jump_cond;
			inst_mult_ex      <= inst_mult;
			inst_multu_ex     <= inst_multu;
			inst_div_ex       <= inst_div;
			inst_divu_ex      <= inst_divu;
			mult_done         <= 1'b0;
			ALUsrcb_ex_unsigned <=  ALUsrcb_id_unsigned;
			if(inst_mthi_ex)
			  hi <= rs_data_ex;
			if(inst_mtlo_ex)
			  lo <= rs_data_ex;
    end
		if((inst_mult_ex || inst_multu_ex)&&!mult_done)
		begin
		  mult_done <= 1'b1;
		end
		if(mult_done)
		begin
		  hi <= mul_res[63:32];
			lo <= mul_res[31:0];
		end
		if(complete)
		begin
		  hi <= div_r;
			lo <= div_s;
		end
end

assign use_mem_rs = (rs_ex == waddr_mem) &&/* (!inst_lw_mem)  && */(rs_ex != 5'd0) && RegWrite_mem;
assign use_wb_rs  = (rs_ex == waddr_wb)  && (rs_ex != 5'd0) && RegWrite_wb;
assign use_mem_rt = (rt_ex == waddr_mem) &&/* (!inst_lw_mem)  && */(rt_ex != 5'd0) && RegWrite_mem;
assign use_wb_rt  = (rt_ex == waddr_wb)  && (rt_ex != 5'd0) && RegWrite_wb;
assign rs_data_ex = use_mem_rs ? (inst_lw_mem ? data_sram_rdata : (wdata_src_mem==4'b0000 ? Result_mem : (wdata_src_mem==4'b0010 ? {Result_mem[15:0],16'd0}
                  : (wdata_src_mem==4'b0011 ? PC_mem+32'd8 : (wdata_src_mem==4'b0100 ? hi :/* wdata_src_wb==4'b1000 ? */lo))))) : (use_wb_rs ? wdata_wb : rdata1_ex);
assign rt_data_ex = use_mem_rt ? (inst_lw_mem ? data_sram_rdata : (wdata_src_mem==4'b0000 ? Result_mem : (wdata_src_mem==4'b0010 ? {Result_mem[15:0],16'd0}
                  : (wdata_src_mem==4'b0011 ? PC_mem+32'd8 : (wdata_src_mem==4'b0100 ? hi :/* wdata_src_wb==4'b1000 ? */lo))))) : (use_wb_rt ? wdata_wb : rdata2_ex);

assign next_PC     = PCsrc_ex==2'b00 ? next_PC_tmp : (PCsrc_ex==2'b10 ? {PC_ex[31:28],inst_index_ex,2'b00} : rs_data_ex);
assign next_PC_tmp = (bran_taken ? PC_ex+32'd4 : PC_ex) + PC_op;
assign PC_op       = bran_taken ? ({{14{offset_ex[15]}},offset_ex,2'd0}) : 32'd4;
assign bran_taken  = bran_ex & taken;
assign taken       = bran_ex ? (inst_beq_ex ? rs_data_ex==rt_data_ex : (inst_bne_ex ? rs_data_ex!=rt_data_ex : 1'b0)) : 1'b0;

assign A     = ALUsrca_ex ? sa_ex : rs_data_ex;
assign B     = ALUsrcb_ex ? ALUsrcb_ex_unsigned ? {16'd0,offset_ex} : {{16{offset_ex[15]}},offset_ex} : rt_data_ex;

assign div_en = inst_div_ex || inst_divu_ex;

// pipeline MEM
//reg  [31:0] Result_mem;
//reg         inst_lw_mem;
reg         inst_sw_mem;
//reg  [31:0] rdata2_mem;
//reg  [31:0] PC_mem;
//reg  [4:0]  waddr_mem;
//reg  [3:0]  wdata_src_mem;
reg         data_sram_en_mem;
reg         pipe4_valid;
//reg         RegWrite_mem;
//wire [31:0] wdata_mem;
reg  [31:0] rt_data_mem;
wire        pipe4_ready_go;
wire        pipe4_to_pipe5_valid;
wire        pipe5_allowin;

assign pipe4_ready_go       = pipe4_valid;
assign pipe4_allowin        = !pipe4_valid || pipe4_ready_go && pipe5_allowin;//1'b1;
assign pipe4_to_pipe5_valid = pipe4_valid && pipe4_ready_go;

always@(posedge clk)
begin
    if(!resetn)
	  begin
      pipe4_valid      <= 1'b0;
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
//	  	rdata2_mem       <= rdata2_ex;
      rt_data_mem      <= rt_data_ex;
	  	PC_mem           <= PC_ex;
	  	waddr_mem        <= waddr_ex;
	  	wdata_src_mem    <= wdata_src_ex;
	  	RegWrite_mem     <= RegWrite_ex;
	  	data_sram_en_mem <= data_sram_en_ex;
    end
end

assign data_sram_addr  = Result;
assign data_sram_wen   = inst_sw_ex ? 4'b1111 : 4'b0000;
assign data_sram_en    = pipe3_valid & data_sram_en_ex;
assign data_sram_wdata = rt_data_ex;//rdata2_mem;

// pipeline WB
reg  [31:0] PC_wb;
//reg  [4:0]  waddr_wb;
//reg  [31:0] wdata_wb;
//reg         RegWrite_wb;
reg         pipe5_valid;
reg  [3:0]  wdata_src_wb;
//reg  [31:0] wdata_from_mem;
reg  [31:0] Result_wb;

assign pipe5_allowin =/* !pipe5_valid || pipe5_ready_go && pipe4_allow_in;//*/1'b1;

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
    PC_wb          <= PC_mem;
  	waddr_wb       <= waddr;
    wdata_wb       <= wdata;
    RegWrite_wb    <= RegWrite;
		wdata_src_wb   <= wdata_src_mem;
//		wdata_from_mem <= data_sram_rdata;
		Result_wb      <= Result_mem;
  end
  else
    RegWrite_wb <= 1'b0;
end

assign wdata    = wdata_src_mem==4'b0000  ? Result_mem   : (wdata_src_mem==4'b0010 ? {Result_mem[15:0],16'd0}
                : (wdata_src_mem==4'b0011 ? PC_mem+32'd8 : wdata_src_mem==4'b0100 ? hi : wdata_src_mem==4'b1000 ? lo : data_sram_rdata));
assign RegWrite = pipe4_valid & RegWrite_mem;
assign waddr    = waddr_mem;

//Debug
wire [31:0] debug_wb_pc;
wire [3:0]  debug_wb_rf_wen;
wire [4:0]  debug_wb_rf_wnum;
wire [31:0] debug_wb_rf_wdata;

assign debug_wb_pc       = RegWrite_wb ? PC_wb : debug_wb_pc;
assign debug_wb_rf_wen   = RegWrite_wb ? waddr_wb == 5'd0 ? 4'd0 : {4{RegWrite_wb}} : 4'b0000;
assign debug_wb_rf_wnum  = RegWrite_wb ? waddr_wb : debug_wb_rf_wnum;
assign debug_wb_rf_wdata = RegWrite_wb ? wdata_wb : debug_wb_rf_wdata;

endmodule
