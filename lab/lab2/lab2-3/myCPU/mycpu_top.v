`timescale 10 ns / 1 ns

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
wire [63:0] mul_res;
reg  [31:0] hi;
reg  [31:0] lo;
wire        div_en;
wire        mul_en;
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
reg  [31:0] IR_tmp;
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
	else if(inst_sram_en || inst_jump_ex || inst_jump_cond_ex || div_en || mul_en)
	begin
		inst_sram_addr <= (inst_jump_ex || (inst_jump_cond_ex && bran_taken)) ? next_PC : /*inst_sram_addr + 32'd4;*/
		                  ((inst_jump_cond_ex && !bran_taken) || div_en&&!complete || mul_en&&!mult_done ? inst_sram_addr : inst_sram_addr + 32'd4);
	end
end

//decode
wire inst_lui ; wire inst_addu; wire inst_addiu; wire inst_subu  ; wire inst_slt   ; wire inst_sltu; wire inst_and  ;
wire inst_or  ; wire inst_xor ; wire inst_nor  ; wire inst_sll   ; wire inst_srl   ; wire inst_sra ; wire inst_lw   ;
wire inst_lb  ; wire inst_lh  ; wire inst_lbu  ; wire inst_lhu   ; wire inst_lwl   ; wire inst_lwr ; wire inst_sb   ;
wire inst_sh  ; wire inst_swl ; wire inst_swr  ; wire inst_sw    ; wire inst_beq   ; wire inst_bne ; wire inst_bgez ;
wire inst_bgtz; wire inst_blez; wire inst_bltz ; wire inst_bltzal; wire inst_bgezal; wire inst_jal ; wire inst_jalr ;
wire inst_jr  ; wire inst_j   ; wire inst_addi ; wire inst_add   ; wire inst_sub   ; wire inst_slti; wire inst_sltiu;
wire inst_andi; wire inst_ori ; wire inst_xori ; wire inst_sllv  ; wire inst_srav  ; wire inst_srlv; wire inst_div  ;
wire inst_divu; wire inst_mult; wire inst_multu; wire inst_mfhi  ; wire inst_mflo  ; wire inst_mthi; wire inst_mtlo ;

wire       bran_id;
wire [1:0] ALUsrcb_id;
wire [1:0] ALUsrca_id;
wire [1:0] waddr_src_id;
wire [3:0] wdata_src_id;
wire       RegWrite_id;
wire       data_sram_en_id;
wire [1:0] PCsrc_id;
wire [3:0] ALUci_id;
wire       ALUsrcb_id_unsign;
wire [7:0] inst_bran;
wire [6:0] inst_load;
wire [4:0] inst_store;

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
assign inst_lb        = (opcode==6'b100000);
assign inst_lbu       = (opcode==6'b100100);
assign inst_lh        = (opcode==6'b100001);
assign inst_lhu       = (opcode==6'b100101);
assign inst_lwl       = (opcode==6'b100010);
assign inst_lwr       = (opcode==6'b100110);
assign inst_sw        = (opcode==6'b101011);
assign inst_sb        = (opcode==6'b101000);
assign inst_sh        = (opcode==6'b101001);
assign inst_swl       = (opcode==6'b101010);
assign inst_swr       = (opcode==6'b101110);
assign inst_beq       = (opcode==6'b000100);
assign inst_bne       = (opcode==6'b000101);
assign inst_bgez      = (opcode==6'b000001) && (rt==5'b00001);
assign inst_bgtz      = (opcode==6'b000111) && (rt==5'b00000);
assign inst_blez      = (opcode==6'b000110) && (rt==5'b00000);
assign inst_bltz      = (opcode==6'b000001) && (rt==5'b00000);
assign inst_bgezal    = (opcode==6'b000001) && (rt==5'b10001);
assign inst_bltzal    = (opcode==6'b000001) && (rt==5'b10000);
assign inst_jal       = (opcode==6'b000011);
assign inst_jalr      = (opcode==6'b000000) && (rt==5'b00000) && (sa==5'b00000) && (func==6'b001001);
assign inst_jr        = (opcode==6'b000000) && (IR[20:11]==10'd0) && (sa==5'b00000) && (func==6'b001000);
assign inst_j         = (opcode==6'b000010);
assign inst_div       = (opcode==6'b000000) && (IR[15:6]==10'd0)  && (func==6'b011010);
assign inst_divu      = (opcode==6'b000000) && (IR[15:6]==10'd0)  && (func==6'b011011);
assign inst_mult      = (opcode==6'b000000) && (IR[15:6]==10'd0)  && (func==6'b011000);
assign inst_multu     = (opcode==6'b000000) && (IR[15:6]==10'd0)  && (func==6'b011001);
assign inst_mfhi      = (opcode==6'b000000) && (IR[25:16]==10'd0) && (sa==5'b00000) && (func==6'b010000);
assign inst_mflo      = (opcode==6'b000000) && (IR[25:16]==10'd0) && (sa==5'b00000) && (func==6'b010010);
assign inst_mthi      = (opcode==6'b000000) && (IR[20:6]==15'd0) && (func==6'b010001);
assign inst_mtlo      = (opcode==6'b000000) && (IR[20:6]==15'd0) && (func==6'b010011);
assign inst_jump      = inst_jal | inst_jr  | inst_j    | inst_jalr;
assign inst_jump_cond = inst_beq | inst_bne | inst_bgez | inst_bgtz | inst_blez | inst_bltz | inst_bgezal | inst_bltzal;


assign bran_id         = inst_beq    | inst_bne    | inst_bgez   | inst_bgtz   | inst_blez  | inst_bltz
                       | inst_bgezal | inst_bltzal;
assign ALUsrcb_id      = inst_lui    | inst_addiu  | inst_addi   | inst_slti   | inst_sltiu | inst_andi  | inst_ori
                       | inst_xori   | inst_lw     | inst_sw     | inst_lb     | inst_lbu   | inst_lh    | inst_lhu
											 | inst_lwl    | inst_lwr    | inst_sb     | inst_sh     | inst_swl   | inst_swr;
assign ALUsrca_id      = inst_sll    | inst_srl    | inst_sra;
assign waddr_src_id[0] = inst_lui    | inst_addiu  | inst_addi   | inst_slti   | inst_sltiu | inst_andi  | inst_ori
                       | inst_xori   | inst_lw     | inst_lb     | inst_lbu    | inst_lh    | inst_lhu   | inst_lwl   | inst_lwr;
assign waddr_src_id[1] = inst_jal    | inst_bgezal | inst_bltzal;
assign wdata_src_id[0] = inst_lw     | inst_lb     | inst_lbu    | inst_lh     | inst_lhu  | inst_lwl   | inst_lwr
                       | inst_jal    | inst_bgezal | inst_bltzal | inst_jalr;
assign wdata_src_id[1] = inst_lui    | inst_jal    | inst_bgezal | inst_bltzal | inst_jalr;
assign wdata_src_id[2] = inst_mfhi;
assign wdata_src_id[3] = inst_mflo;
assign RegWrite_id     = ~inst_sw    & ~inst_sb    & ~inst_sh    & ~inst_swl   & ~inst_swr  & ~inst_beq  & ~inst_bne
                       & ~inst_bgez  & ~inst_bgtz  & ~inst_blez  & ~inst_bltz  & ~inst_jr   & ~inst_j    & ~inst_mthi & ~inst_mtlo;
assign data_sram_en_id = inst_lw     | inst_sw     | inst_lb     | inst_lbu    | inst_lh    | inst_lhu   | inst_lwl
                       | inst_lwr    | inst_sb     | inst_sh     | inst_swl    | inst_swr;
assign PCsrc_id[0]     = inst_jr     | inst_jalr;
assign PCsrc_id[1]     = inst_jal    | inst_j;
assign ALUci_id[0]     = inst_slt    | inst_slti   | inst_or     | inst_ori    | inst_nor   | inst_sll   | inst_sllv
                       | inst_srl    | inst_srlv   | inst_sra    | inst_srav;
assign ALUci_id[1]     = inst_lui    | inst_addu   | inst_add    | inst_addiu  | inst_addi  | inst_subu  | inst_sub
                       | inst_slt    | inst_slti   | inst_xor    | inst_xori   | inst_srl   | inst_srlv  | inst_sra
											 | inst_srav   | inst_lw     | inst_sw     | inst_lb     | inst_lbu   | inst_lh    | inst_lhu
											 | inst_lwl    | inst_lwr    | inst_sb     | inst_sh     | inst_swl   | inst_swr;
assign ALUci_id[2]     = inst_subu   | inst_sub    | inst_slt    | inst_slti   | inst_sltu  | inst_sltiu | inst_sll   | inst_sllv;
assign ALUci_id[3]     = inst_xor    | inst_xori   | inst_nor    | inst_sra    | inst_srav;
assign inst_bran[0]    = inst_beq;
assign inst_bran[1]    = inst_bne;
assign inst_bran[2]    = inst_bgez;
assign inst_bran[3]    = inst_bgtz;
assign inst_bran[4]    = inst_blez;
assign inst_bran[5]    = inst_bltz;
assign inst_bran[6]    = inst_bgezal;
assign inst_bran[7]    = inst_bltzal;
assign inst_load[0]    = inst_lb;
assign inst_load[1]    = inst_lbu;
assign inst_load[2]    = inst_lh;
assign inst_load[3]    = inst_lhu;
assign inst_load[4]    = inst_lwl;
assign inst_load[5]    = inst_lwr;
assign inst_load[6]    = inst_lw;
assign inst_store[0]   = inst_sb;
assign inst_store[1]   = inst_sh;
assign inst_store[2]   = inst_swl;
assign inst_store[3]   = inst_swr;
assign inst_store[4]   = inst_sw;

assign ALUsrcb_id_unsign = inst_andi | inst_ori | inst_xori;

//pipeline IF
reg        pipe1_valid;
reg        inst_sram_en;
wire       pipe1_ready_go;
wire       pipe1_to_pipe2_valid;
wire       pipe2_allowin;
reg        pipe2_valid;
reg        ir_cancel;
reg        ir_stay;

assign pipe1_ready_go       = div_en ? complete : (mul_en ? mult_done : !(inst_jump_ex || inst_jump_cond_ex));
assign pipe1_allowin        = !pipe1_valid || pipe1_ready_go && pipe2_allowin;
assign pipe1_to_pipe2_valid = pipe1_valid && pipe1_ready_go;

always@(posedge clk)
begin
  if(!resetn)
  begin
    pipe1_valid  <= 1'b0;
	  inst_sram_en <= 1'b0;
	  ir_cancel <= 1'b0;
  end
  else if(((pipe1_allowin && !(inst_jump || inst_jump_cond) || !pipe1_ready_go))&&!div_en&&!mul_en || complete || mult_done)
  begin
    inst_sram_en <= 1'b1;
	  pipe1_valid  <= 1'b1;
  end

  else if(inst_jump ||inst_jump_cond || (mul_en&&!mult_done) || (div_en&&!complete))
  begin
	  inst_sram_en <= 1'b0;
//	pipe1_valid  <= 1'b0;
  end
	else
	  inst_sram_en <= 1'b0;
	if(inst_jump_ex || inst_jump_cond_ex)
	  ir_cancel <= 1'b1;
  if(ir_cancel)
    ir_cancel <= 1'b0;
end

always @(posedge clk)
begin
  if(!resetn)
  begin
    IR_tmp <= 32'd0;
    ir_stay <= 1'b0;
  end
  else
  begin
    if((mul_en || div_en) && inst_sram_en)
    begin
      IR_tmp <= inst_sram_rdata;
      ir_stay <= 1'b1;
    end
    else if(complete || mult_done)
      ir_stay <= 1'b0;
  end
end

assign IR = ir_cancel ? 32'd0 : ir_stay ? IR_tmp : inst_sram_rdata;

// pipeline ID
reg  [31:0] PC_id;
reg  [1:0]  PCsrc_ex;
reg  [31:0] PC_ex;
reg         bran_ex;
reg  [15:0] offset_ex;
wire [31:0] next_PC_tmp;
wire [4:0]  waddr_id;
wire        pipe2_ready_go;
wire        pipe2_to_pipe3_valid;
wire        pipe3_allowin;
wire        taken;

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
    end
end

assign raddr1      = rs;
assign raddr2      = rt;
assign waddr_id    = waddr_src_id==2'b01 ? rt : (waddr_src_id==2'b10 ? 5'd31 : rd);

// pipeline EX
reg  [7:0]  inst_bran_ex;
reg  [6:0]  inst_load_ex;
reg  [4:0]  inst_store_ex;
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
reg  [25:0] inst_index_ex;
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
reg         ALUsrcb_ex_unsign;
reg         RegWrite_mem;
reg         RegWrite_wb;
reg  [31:0] wdata_wb;
reg         inst_mthi_ex;
reg         inst_mtlo_ex;
wire [31:0] dram_to_reg;

assign pipe3_ready_go       = div_en ? complete : (mul_en ? mult_done : pipe3_valid);
assign pipe3_allowin        = !pipe3_valid || pipe3_ready_go && pipe4_allowin;
assign pipe3_to_pipe4_valid = pipe3_valid && pipe3_ready_go;

always@(posedge clk)
begin
    if(!resetn)
    begin
      pipe3_valid       <= 1'b0;
      inst_jump_ex      <= 1'b0;
      inst_jump_cond_ex <= 1'b0;
			mult_done         <= 1'b0;
			bran_ex           <= 1'b0;
			inst_div_ex       <= 1'b0;
			inst_divu_ex      <= 1'b0;
			inst_mult_ex      <= 1'b0;
			inst_multu_ex     <= 1'b0;
			data_sram_en_ex   <= 1'b0;
    end
    else if(pipe3_allowin)
	  begin
      pipe3_valid <= pipe2_to_pipe3_valid;
    end

    if(pipe2_to_pipe3_valid && pipe3_allowin)
		begin
	    inst_bran_ex      <= inst_bran;
			inst_load_ex      <= inst_load;
			inst_store_ex     <= inst_store;
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
			inst_mult_ex      <= mult_done ? 1'b0 : inst_mult;
			inst_multu_ex     <= mult_done ? 1'b0 : inst_multu;
			inst_div_ex       <= complete ? 1'b0 : inst_div;
			inst_divu_ex      <= complete ? 1'b0 : inst_divu;
			mult_done         <= 1'b0;
			ALUsrcb_ex_unsign <= ALUsrcb_id_unsign;
			if(inst_mthi_ex)
			  hi <= rs_data_ex;
			if(inst_mtlo_ex)
			  lo <= rs_data_ex;
    end
		if(mul_en&&!mult_done)
		begin
		  mult_done <= 1'b1;
		end
		if(mult_done)
		begin
		  hi <= mul_res[63:32];
			lo <= mul_res[31:0];
			inst_mult_ex <= 1'b0;
			inst_multu_ex <= 1'b0;
			mult_done <= 1'b0;
		end
		if(complete)
		begin
		  hi <= div_r;
			lo <= div_s;
			inst_div_ex  <= 1'b0;
			inst_divu_ex <= 1'b0;
		end
end

/*data relevent*/
assign use_mem_rs = (rs_ex == waddr_mem) && (rs_ex != 5'd0) && RegWrite_mem;
assign use_wb_rs  = (rs_ex == waddr_wb)  && (rs_ex != 5'd0) && RegWrite_wb;
assign use_mem_rt = (rt_ex == waddr_mem) && (rt_ex != 5'd0) && RegWrite_mem;
assign use_wb_rt  = (rt_ex == waddr_wb)  && (rt_ex != 5'd0) && RegWrite_wb;
assign rs_data_ex = use_mem_rs ? (wdata_src_mem==4'b0000 ? Result_mem : (wdata_src_mem==4'b0010 ? {Result_mem[15:0],16'd0}
                  : (wdata_src_mem==4'b0011 ? PC_mem+32'd8 : (wdata_src_mem==4'b0100 ? hi : wdata_src_mem==4'b1000 ? lo : dram_to_reg)))) : (use_wb_rs ? wdata_wb : rdata1_ex);
assign rt_data_ex = use_mem_rt ? (wdata_src_mem==4'b0000 ? Result_mem : (wdata_src_mem==4'b0010 ? {Result_mem[15:0],16'd0}
                  : (wdata_src_mem==4'b0011 ? PC_mem+32'd8 : (wdata_src_mem==4'b0100 ? hi : wdata_src_mem==4'b1000 ? lo : dram_to_reg)))) : (use_wb_rt ? wdata_wb : rdata2_ex);

/*jump concerning*/
assign next_PC     = PCsrc_ex==2'b00 ? next_PC_tmp : (PCsrc_ex==2'b10 ? {PC_id[31:28],inst_index_ex,2'b00} : rs_data_ex);
assign next_PC_tmp = PC_id + ({{14{offset_ex[15]}},offset_ex,2'd0});
assign bran_taken  = bran_ex & taken;
assign taken       = bran_ex ? (inst_bran_ex[0] && rs_data_ex==rt_data_ex) || (inst_bran_ex[1] && rs_data_ex!=rt_data_ex )
                   || ((inst_bran_ex[2] || inst_bran_ex[6]) && !rs_data_ex[31])    || (inst_bran_ex[3]  && !rs_data_ex[31]  && rs_data_ex!=32'd0)
									 || (inst_bran_ex[4]  && (rs_data_ex[31]  || rs_data_ex==32'd0)) || ((inst_bran_ex[5] || inst_bran_ex[7]) && rs_data_ex[31])
                   : 1'b0;

/*operators for ALU*/
assign A     = ALUsrca_ex ? sa_ex : rs_data_ex;
assign B     = ALUsrcb_ex ? ALUsrcb_ex_unsign ? {16'd0,offset_ex} : {{16{offset_ex[15]}},offset_ex} : rt_data_ex;

/*div and mul*/
assign div_en = inst_div_ex  || inst_divu_ex;
assign mul_en = inst_mult_ex || inst_multu_ex;

// pipeline MEM
reg  [6:0]  inst_load_mem;
reg  [3:0]  ea_mem;
reg         pipe4_valid;
reg  [31:0] rt_data_mem;
wire        pipe4_ready_go;
wire        pipe4_to_pipe5_valid;
wire        pipe5_allowin;
wire [3:0]  ea;

assign pipe4_ready_go       = pipe4_valid;
assign pipe4_allowin        = !pipe4_valid || pipe4_ready_go && pipe5_allowin;//1'b1;
assign pipe4_to_pipe5_valid = pipe4_valid && pipe4_ready_go;

always@(posedge clk)
begin
    if(!resetn)
	  begin
      pipe4_valid      <= 1'b0;
      RegWrite_mem     <= 1'b0;
    end
    else if(pipe4_allowin)
  	begin
      pipe4_valid <= pipe3_to_pipe4_valid;
    end

    if(pipe3_to_pipe4_valid && pipe4_allowin)
	  begin
		  Result_mem       <= Result;
			ea_mem           <= ea;
		  inst_load_mem    <= inst_load_ex;
      rt_data_mem      <= rt_data_ex;
	  	PC_mem           <= PC_ex;
	  	waddr_mem        <= waddr_ex;
	  	wdata_src_mem    <= wdata_src_ex;
	  	RegWrite_mem     <= RegWrite_ex;
    end
end

assign ea[0]           = Result[1:0]==2'b00;
assign ea[1]           = Result[1:0]==2'b01;
assign ea[2]           = Result[1:0]==2'b10;
assign ea[3]           = Result[1:0]==2'b11;
assign data_sram_addr  = {Result[31:2],2'b0};
assign data_sram_en    = pipe3_valid & data_sram_en_ex;

assign data_sram_wen   = ({4{inst_store_ex[0] & ea[0]}} & 4'h1) | ({4{inst_store_ex[0] & ea[1]}} & 4'h2)                 /*sb*/
                       | ({4{inst_store_ex[0] & ea[2]}} & 4'h4) | ({4{inst_store_ex[0] & ea[3]}} & 4'h8)
                       | ({4{inst_store_ex[1] & ea[0]}} & 4'h3) | ({4{inst_store_ex[1] & ea[2]}} & 4'hc)                 /*sh*/
                       | ({4{inst_store_ex[4]}} & 4'hf)                                                                  /*sw*/
                       | ({4{ea[0] & inst_store_ex[2]}} & 4'h1) | ({4{ea[1] & inst_store_ex[2]}} & 4'h3)                /*swl*/
											 | ({4{ea[2] & inst_store_ex[2]}} & 4'h7) | ({4{ea[3] & inst_store_ex[2]}} & 4'hf)
											 | ({4{ea[0] & inst_store_ex[3]}} & 4'hf) | ({4{ea[1] & inst_store_ex[3]}} & 4'he)                /*swr*/
											 | ({4{ea[2] & inst_store_ex[3]}} & 4'hc) | ({4{ea[3] & inst_store_ex[3]}} & 4'h8);

assign data_sram_wdata = ({32{inst_store_ex[0] & ea[0]}} & {24'd0,rt_data_ex[7:0]}) | ({32{inst_store_ex[0] & ea[1]}} & {16'd0,rt_data_ex[7:0],8'd0})       /*sb*/
                       | ({32{inst_store_ex[0] & ea[2]}} & {8'd0,rt_data_ex[7:0],16'd0}) | ({32{inst_store_ex[0] & ea[3]}} & {rt_data_ex[7:0],24'd0})
                       | ({32{inst_store_ex[1] & ea[0]}} & {16'd0,rt_data_ex[15:0]}) | ({32{inst_store_ex[1] & ea[2]}} & {rt_data_ex[15:0],16'd0})         /*sh*/
                       | ({32{ea[0] & inst_store_ex[2]}} & {24'd0,rt_data_ex[31:24]}) | ({32{ea[1] & inst_store_ex[2]}} & {16'd0,rt_data_ex[31:16]})       /*swl*/
											 | ({32{ea[2] & inst_store_ex[2]}} & {8'd0,rt_data_ex[31:8]}) | ({32{ea[3] & inst_store_ex[2]}} & rt_data_ex)
											 | ({32{ea[0] & inst_store_ex[3]}} & rt_data_ex) | ({32{ea[1] & inst_store_ex[3]}} & {rt_data_ex[23:0],8'd0})                        /*swr*/
											 | ({32{ea[2] & inst_store_ex[3]}} & {rt_data_ex[15:0],16'd0}) | ({32{ea[3] & inst_store_ex[3]}} & {rt_data_ex[7:0],24'd0})
											 | ({32{inst_store_ex[4]}} & rt_data_ex);                                                                                            /*sw*/

assign dram_to_reg = ({32{ea_mem[0] & inst_load_mem[0]}} & {{24{data_sram_rdata[7]}},data_sram_rdata[7:0]})    | ({32{ea_mem[1] & inst_load_mem[0]}} & {{24{data_sram_rdata[15]}},data_sram_rdata[15:8]})       /*lb*/
                   | ({32{ea_mem[2] & inst_load_mem[0]}} & {{24{data_sram_rdata[23]}},data_sram_rdata[23:16]}) | ({32{ea_mem[3] & inst_load_mem[0]}} & {{24{data_sram_rdata[31]}},data_sram_rdata[31:24]})
									 | ({32{ea_mem[0] & inst_load_mem[1]}} & {24'd0,data_sram_rdata[7:0]})                       | ({32{ea_mem[1] & inst_load_mem[1]}} & {24'd0,data_sram_rdata[15:8]})                          /*lbu*/
									 | ({32{ea_mem[2] & inst_load_mem[1]}} & {24'd0,data_sram_rdata[23:16]})                     | ({32{ea_mem[3] & inst_load_mem[1]}} & {24'd0,data_sram_rdata[31:24]})
									 | ({32{ea_mem[0] & inst_load_mem[2]}} & {{16{data_sram_rdata[15]}},data_sram_rdata[15:0]})  | ({32{ea_mem[2] & inst_load_mem[2]}} & {{16{data_sram_rdata[31]}},data_sram_rdata[31:16]})      /*lh*/
									 | ({32{ea_mem[0] & inst_load_mem[3]}} & {16'd0,data_sram_rdata[15:0]})                      | ({32{ea_mem[2] & inst_load_mem[3]}} & {16'd0,data_sram_rdata[31:16]})                         /*lhu*/
                   | ({32{ea_mem[0] & inst_load_mem[4]}} & {data_sram_rdata[7:0],rt_data_mem[23:0]})           | ({32{ea_mem[1] & inst_load_mem[4]}} & {data_sram_rdata[15:0],rt_data_mem[15:0]})               /*lwl*/
                   | ({32{ea_mem[2] & inst_load_mem[4]}} & {data_sram_rdata[23:0],rt_data_mem[7:0]})           | ({32{ea_mem[3] & inst_load_mem[4]}} & data_sram_rdata)
									 | ({32{ea_mem[0] & inst_load_mem[5]}} & data_sram_rdata)                                    | ({32{ea_mem[1] & inst_load_mem[5]}} & {rt_data_mem[31:24],data_sram_rdata[31:8]})              /*lwr*/
                   | ({32{ea_mem[2] & inst_load_mem[5]}} & {rt_data_mem[31:16],data_sram_rdata[31:16]})        | ({32{ea_mem[3] & inst_load_mem[5]}} & {rt_data_mem[31:8],data_sram_rdata[31:24]})
                   | ({32{inst_load_mem[6]}} & data_sram_rdata);                                                                                                                                        /*lw*/

// pipeline WB
reg  [31:0] PC_wb;
reg         pipe5_valid;
reg  [31:0] debug_pc;
reg  [4:0]  debug_waddr;
reg  [31:0] debug_wdata;

assign pipe5_allowin =/* !pipe5_valid || pipe5_ready_go && pipe4_allow_in*/1'b1;

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
		debug_pc       <= PC_wb;
		debug_waddr    <= waddr_wb;
		debug_wdata    <= wdata_wb;
  end
  else
    RegWrite_wb <= 1'b0;
end

assign wdata    = wdata_src_mem==4'b0000  ? Result_mem   : (wdata_src_mem==4'b0010 ? {Result_mem[15:0],16'd0}
                : (wdata_src_mem==4'b0011 ? PC_mem+32'd8 : wdata_src_mem==4'b0100  ? hi : wdata_src_mem==4'b1000 ? lo : dram_to_reg));
assign RegWrite = pipe4_valid & RegWrite_mem;
assign waddr    = waddr_mem;

//Debug
assign debug_wb_pc       = RegWrite_wb ? PC_wb : debug_pc;
assign debug_wb_rf_wen   = RegWrite_wb ? waddr_wb == 5'd0 ? 4'd0 : {4{RegWrite_wb}} : 4'b0000;
assign debug_wb_rf_wnum  = RegWrite_wb ? waddr_wb : debug_waddr;
assign debug_wb_rf_wdata = RegWrite_wb ? wdata_wb : debug_wdata;

endmodule
