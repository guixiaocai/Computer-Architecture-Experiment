`timescale 10ns / 1ns

`define IF  3'b000
//`define IW  3'b001
`define ID  3'b010
`define EX  3'b111
`define ST  3'b011
`define LD  3'b100
//`define RDW 3'b101
`define WB  3'b110


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
	output inst_sram_en,
	output [3:0] inst_sram_wen,
	input  [31:0] inst_sram_rdata,
	output [31:0] inst_sram_addr,
	output [31:0] inst_sram_wdata,

	//Data_Sram
	output [3:0] data_sram_wen,
	output data_sram_en,
	output [31:0] data_sram_wdata,
	output [31:0] data_sram_addr,
	input [31:0] data_sram_rdata,

	//Debug_Signal
	output [31:0] debug_wb_pc,
	output [3:0] debug_wb_rf_wen,
	output [4:0] debug_wb_rf_wnum,
	output [31:0] debug_wb_rf_wdata
);

  //TODO: Please add your MIPS CPU code here

wire [31:0] A;
wire [31:0] B;
reg  [3:0] ALUci;
wire Overflow;
wire CarryOut;
wire Zero;
wire [31:0] Result;
wire [31:0] res1;
wire [31:0] res2;
wire [31:0] res3;
wire [4:0] waddr;
wire [4:0] raddr1;
wire [4:0] raddr2;
wire [31:0] wdata;
wire [31:0] rdata1;
wire [31:0] rdata2;
wire [31:0] next_PC1;
wire [31:0] next_PC2;
wire [31:0] next_PC;
wire [1:0] ea;
wire [4:0] rs;
wire [4:0] rt;
wire [4:0] rd;
wire [5:0] opcode;
wire [5:0] func;
wire [31:0] Write_data;
wire [3:0] Write_strb;
wire [2:0] Wreg_strb;

wire Mark_B;
wire Mark_B1;
wire [31:0] Read_data_tmp;
wire [31:0] Read_data_tmp1;
wire [31:0] Read_data_tmp2;
wire [31:0] Read_data_LWL;
wire [31:0] Read_data_LB;
wire [31:0] Read_data_LH;
wire [31:0] Write_data_tmp1;
wire [31:0] Write_data_tmp2;
wire [31:0] Write_data_SWL;
wire [31:0] Write_data_SB;
wire [31:0] Write_data_SH;
wire [3:0] Write_strb_tmp1;
wire [3:0] Write_strb_tmp2;
wire [3:0] Write_strb_SWL;
wire [3:0] Write_strb_SB;
wire [3:0] Write_strb_SH;

reg [31:0] inst_sram_addr;
reg [31:0] PC;
reg inst_sram_en;
reg data_sram_en;
reg RegDst;
reg MemtoReg;
reg Branch;
wire PCsrc;
reg Jump;
reg RegWrite;
reg PCtoReg;
reg RegtoPC;
reg Upper;
reg Signed;
reg LSide;
reg SSide;
reg [1:0] ALUsrc;
reg [1:0] mkPCsrc;
reg [1:0] AccWay;

reg [2:0] curr_state;
reg [2:0] next_state;
wire [31:0] IR;
wire [31:0] MDR;
//wire [5:0] opCode;
//wire [5:0] Func;
reg [31:0] ALUout;

alu u_alu(.A(A),.B(B),.ALUci(ALUci),.Overflow(Overflow),.CarryOut(CarryOut),.Zero(Zero),.Result(Result));

reg_file u_reg_file(.clk(clk),.resetn(resetn),.waddr(waddr),.raddr1(raddr1),.raddr2(raddr2),.wen(RegWrite),
                    .Wreg_strb(Wreg_strb),.wdata(wdata),.rdata1(rdata1),.rdata2(rdata2));


//assign opCode = Instruction[31:26];
//assign Func = Instruction[5:0];
assign inst_sram_wen = 4'd0;
assign inst_sram_wdata = 32'd0;
assign IR = inst_sram_rdata;
assign MDR = data_sram_rdata;
assign opcode = IR[31:26];
assign rs = IR[25:21];
assign rt = IR[20:16];
assign rd = IR[15:11];
assign func = IR[5:0];
assign res1 = inst_sram_addr + 32'd4;
assign res2 = res1 + {{14{IR[15]}},IR[15:0],2'b00};
assign res3 = (curr_state==`ID) ? (inst_sram_addr + 32'd8) : res3;
assign next_PC1 = PCsrc ? res2 : res1;
assign next_PC2 = RegtoPC ? rdata1 : next_PC1;
assign next_PC = Jump ? {res1[31:28],IR[25:0],2'b00} : next_PC2;

always@(posedge clk)
begin
	if(!resetn)
		inst_sram_addr <= 32'hbfc00000;
	else if(curr_state==`EX)
	begin
		inst_sram_addr <= next_PC;
	end
	else
		inst_sram_addr <= inst_sram_addr;
end

always@(posedge clk)
begin
	if(!resetn)
		curr_state <= `IF;
	else
		curr_state <= next_state;
end

always@(*)
begin
	case(curr_state)
	`IF: next_state = `ID;
	`ID: next_state = `EX;
	`EX:
	begin
		if(opcode==`lw||opcode==`lwl||opcode==`lwr||opcode==`lh
		 ||opcode==`lhu||opcode==`lb||opcode==`lbu)
			next_state = `LD;
		else if(opcode==`sw||opcode==`swl||opcode==`swr
		      ||opcode==`sh||opcode==`sb)
			next_state = `ST;
		else if(opcode==`j||/*opcode==`jal||*/opcode==`bne||opcode==`beq||opcode==`bgtz||opcode==`bltz/*||opcode==`bgez*/
		      ||opcode==`blez/*||((opcode==6'b000000)&&(func==`jr||func==`jalr))*/)
			next_state = `IF;
		else
			next_state = `WB;
	end
	`ST:  next_state = `IF;
	`LD:  next_state = `WB;
	`WB:  next_state = `IF;
	default: next_state = `IF;
	endcase
end

always@(posedge clk)
begin
	if(!resetn)
	begin
		inst_sram_en <= 1;
		data_sram_en <= 0;
		RegWrite     <= 0;
		RegDst       <= 0;
		ALUsrc       <= 2'b00;
		MemtoReg     <= 0;
		Branch       <= 0;
		ALUci        <= 4'b0000;
		mkPCsrc      <= 2'b00;
		Jump         <= 0;
		RegtoPC      <= 0;
		PCtoReg      <= 0;
		Upper        <= 0;
		AccWay       <= 2'b00;
		Signed       <= 0;
		LSide        <= 0;
		SSide        <= 0;
	end
	else
	begin
		case(curr_state)
		`IF:
		begin
			inst_sram_en <= 0;
			PC <= inst_sram_addr;
		end
		`ID:
		begin
		  inst_sram_en <= 0;
	    case(opcode)
		  6'b001001:          /*addiu*/       /*ALU_ADD*/
		  begin
			  ALUsrc <= 2'b11; ALUci <= 4'b0010;
		  end
		  6'b000101:        /*bne*/           /*ALU_SUB*/
		  begin
			  Branch <= 1; ALUci = 4'b0110; mkPCsrc <= 2'b10;
		  end
		  6'b000100:        /*beq*/          /*ALU_SUB*/
		  begin
			  Branch <= 1; ALUci <= 4'b0110; mkPCsrc <= 2'b11;
		  end
	      6'b000110:
		  begin         /*blez*/
			  Branch <= 1; mkPCsrc <= 2'b01; Signed <= 1;
		  end
		  6'b000111:   /*bgtz*/
		  begin
			  Branch <= 1; mkPCsrc <= 2'b01; Signed <= 1;
		  end
		  6'b000001:
		  begin         /*bgez*//*bltz*/
	      Branch <= 1; mkPCsrc <= 2'b01;
		  end
		  6'b001100:         /*andi*/      /*ALU_AND*/
		  begin
	      ALUsrc <= 2'b10;
		  end
		  6'b001101:        /*ori*/        /*ALU_OR*/
		  begin
		  	ALUsrc <= 2'b10; ALUci <= 4'b0001;
		  end
		  6'b001110:      /*xori*/         /*ALU_XOR*/
		  begin
            ALUsrc <= 2'b10; ALUci <= 4'b1010;
		  end
		  6'b100011:       /*lw*/          /*ALU_ADD*/
		  begin
	        ALUsrc <= 2'b11; MemtoReg <= 1; ALUci <= 4'b0010;
		  end
		  6'b100010:         /*lwl*/      /*ALU_ADD*/
		  begin
			ALUsrc <= 2'b11; MemtoReg <= 1; ALUci <= 4'b0010; AccWay <= 2'b11;
		  end
	    6'b100000:       /*lb*/     /*ALU_ADD*/
	    begin
		    ALUsrc <= 2'b11; MemtoReg <= 1; ALUci <= 4'b0010; AccWay <= 2'b01; Signed <= 1;
	    end
	    6'b100100:     /*lbu*/    /*ALU_ADD*/
  	  begin
	  	  ALUsrc <= 2'b11; MemtoReg <= 1; ALUci <= 4'b0010; AccWay <= 2'b01;
  	  end
	    6'b100001:    /*lh*/    /*ALU_ADD*/
	    begin
		    ALUsrc <= 2'b11; MemtoReg <= 1; ALUci <= 4'b0010; AccWay <= 2'b10; Signed <= 1;
	    end
	    6'b100101:   /*lhu*/        /*ALU_ADD*/
	    begin
		    ALUsrc <= 2'b11; MemtoReg <= 1; ALUci <= 4'b0010; AccWay <= 2'b10;
	    end
	    6'b100110:            /*lwr*/     /*ALU_ADD*/
	    begin
		    ALUsrc <= 2'b11; MemtoReg <= 1; ALUci <= 4'b0010; LSide <= 1; AccWay <= 2'b11;
	    end
	    6'b001111:      /*lui*/           /*ALU_ADD*/
	    begin
        ALUsrc <= 2'b11; ALUci <= 4'b0010; Upper <= 1;
	    end
	    6'b101011:       /*sw*/          /*ALU_ADD*/
	    begin
	      ALUsrc <= 2'b11; ALUci <= 4'b0010;
	    end
	    6'b101010:      /*swl*/           /*ALU_ADD*/
	    begin
	      ALUsrc <= 2'b11; ALUci <= 4'b0010; AccWay <= 2'b11;
	    end
	    6'b101000:        /*sb*/    /*ALU_ADD*/
  	  begin
	      ALUsrc <= 2'b11; ALUci <= 4'b0010; AccWay <= 2'b01;
	    end
	    6'b101001:           /*sh*/      /*ALU_ADD*/
	    begin
	      ALUsrc <= 2'b11; ALUci <= 4'b0010; AccWay <= 2'b10;
	    end
	    6'b101110:         /*swr*/      /*ALU_ADD*/
	    begin
	      ALUsrc <= 2'b11; ALUci <= 4'b0010; SSide <= 1;
	    end
	    6'b001010:       /*slti*/      /*ALU_SLT*/
	    begin
		    ALUsrc <= 2'b11; ALUci <= 4'b0111;
	    end
	    6'b001011:     /*sltiu*/       /*ALU_SLTU*/
	    begin
		    ALUsrc <= 2'b11; ALUci <= 4'b0100;
	    end
  	  6'b000010:      /*j*/
		    Jump <= 1;
	    6'b000011:       /*jal*/
	    begin
		    Jump <= 1; /*PCtoReg <= 1; RegWrite <= 1;*/
	    end
	    6'b000000:       /*special*/
	    begin
		    case(func)
		    6'b000000:        /*nop and sll*/   /*ALU_SL*/
		    begin
			    RegDst <= 1; ALUsrc <= 2'b01; ALUci <= 4'b0101;
		    end
		    6'b000100:        /*sllv*/          /*ALU_SL*/
		    begin
			    RegDst <= 1; ALUci <= 4'b0101;
		    end
		    6'b000011:      /*sra*/          /*ALU_SRA*/
		    begin
			    RegDst <= 1; ALUsrc <= 2'b01; ALUci <= 4'b1011;
		    end
		    6'b000111:     /*srav*/        /*ALU_SRA*/
		    begin
			    RegDst <= 1; ALUci <= 4'b1011;
		    end
		    6'b000010:    /*srl*/          /*ALU_SRL*/
		    begin
			    RegDst <= 1; ALUsrc <= 2'b01; ALUci <= 4'b0011;
	     	end
		    6'b000110:      /*srlv*/         /*ALU_SRL*/
		    begin
			    RegDst <= 1; ALUci <= 4'b0011;
		    end
		    6'b100001:         /*addu*/  /*ALU_ADD*/
		    begin
			    RegDst <= 1; ALUci <= 4'b0010;
	    	end
	      6'b100011:         /*subu*/   /*ALU_SUB*/
		    begin
			    RegDst <= 1; ALUci <= 4'b0110;
		    end
		    6'b100100:        /*and*/      /*ALU_AND*/
		    begin
			    RegDst <= 1; ALUci <= 4'b0000;
		    end
		    6'b100101:          /*or*/ /*move*/    /*ALU_OR*/
		    begin
			    RegDst <= 1; ALUci <= 4'b0001;
		    end
		    6'b100111:          /*nor*/        /*ALU_NOR*/
		    begin
			    RegDst <= 1; ALUci <= 4'b1001;
		    end
	    	6'b100110:         /*xor*/         /*ALU_XOR*/
		    begin
			    RegDst <= 1; ALUci <= 4'b1010;
	    	end
		    6'b001010:        /*movz*/        /*ALU_NOPA*/
		    begin
			    RegDst <= 1; ALUci <= 4'b1100;
		    end
		    6'b001011:        /*movn*/      /*ALU_NOPA*/
		    begin
			    RegDst <= 1; ALUci <= 4'b1100;
		    end
		    6'b001000:        /*jr*/
		    begin
			    RegtoPC <= 1;
		    end
		    6'b001001:       /*jalr*/
		    begin
			    RegtoPC <= 1; PCtoReg <= 1;
		    end
		    6'b101010:       /*slt*/         /*ALU_SLT*/
		    begin
			    RegDst <= 1; ALUci <= 4'b0111;
		    end
		    6'b101011:      /*sltu*/        /*ALU_SLTU*/
		    begin
			    RegDst <= 1; ALUci <= 4'b0100;
		    end
		    default: ;
		    endcase
	    end
	    default: ;
	    endcase
		end
		`EX:
		begin
			ALUout  <= Result;
			ALUsrc  <= 2'b00;
			Branch  <= 0;
			ALUci   <= 4'b0000;
			mkPCsrc <= 2'b00;
			RegtoPC <= 0;
			if(next_state==`ST)
			begin
				data_sram_en <= 1;
				RegWrite     <= 0;
			end
			else if(next_state==`LD)
			begin
				data_sram_en <= 1;
				RegWrite     <= 0;
			end
			else if(next_state==`IF)
			begin
				inst_sram_en <= 1;
				RegWrite     <= 0;
				PCtoReg      <= 0;
				Jump         <= 0;
			end
			else   /*next_state==`WB*/
			begin
				if(opcode==6'b000000&&func==`movn)
				begin
					RegWrite <= (rdata2!=32'd0);
					PCtoReg  <= 0;
				end
				else if(opcode==6'b000000&&func==`movz)
				begin
					RegWrite <= (rdata2==32'd0);
					PCtoReg  <= 0;
				end
				else if(opcode==6'b000000&&func==`jr)
				begin
					RegWrite <= 0;
					PCtoReg  <= 0;
				end
				else if(opcode==6'b000000&&func==`jalr)
				begin
					RegWrite <= 1;
					PCtoReg  <= 1;
				end
				else if(opcode==`jal)
				begin
					RegWrite <= 1;
					PCtoReg  <= 1;
					Jump     <= 1;
				end
				else
				begin
					RegWrite <= 1;
					PCtoReg  <= 0;
				end
			end
		end
		`ST:
		begin   //if(next_state==`IF)
			inst_sram_en <= 1;
			data_sram_en <= 0;
			AccWay       <= 2'b00;
			Signed       <= 0;
			LSide        <= 0;
			SSide        <= 0;
		end
		`LD:
		begin   //	if(next_state==`WB)
			data_sram_en <= 0;
			RegWrite     <= 1;
		end
		`WB:            //next_state==`IF
		begin
			inst_sram_en <= 1;
			RegWrite     <= 0;
			PCtoReg      <= 0;
			RegDst       <= 0;
			MemtoReg     <= 0;
			Upper        <= 0;
			AccWay       <= 2'b00;
			Signed       <= 0;
			LSide        <= 0;
			SSide        <= 0;
			Jump         <= 0;
		end
		default: ;
		endcase
	end
end




//control
	assign raddr1 = rs;
	assign raddr2 = rt;
	assign A = (ALUsrc==2'b01) ? {27'd0,IR[10:6]} : rdata1;//rdata2 : rdata1;

//RegDst
	assign waddr = PCtoReg ? (Jump ? 5'b11111 : rd) : (RegDst ? rd : rt);

//ALUsrc
	assign B = ALUsrc[1] ?  (ALUsrc[0] ? {{16{IR[15]}},IR[15:0]} : {16'd0,IR[15:0]}) : rdata2;

//Address
	assign data_sram_addr = ALUout;//[31:2],2'b00};

//PCsrc
	assign PCsrc = Branch&Mark_B;
	assign Mark_B = mkPCsrc[0] ? (mkPCsrc[1] ? Zero : Mark_B1) : mkPCsrc[1]&~Zero;/*beq*/ /*bne*/
	assign Mark_B1 = Signed ? ((opcode==`blez) ? (rdata1==32'd0)|rdata1[31] :
			          ((rdata1!=32'd0)&~rdata1[31])) : (rt[0] ? ~rdata1[31] : rdata1[31]);  /*blez*//*bgez*//*bltz*/

//MemRead&MemtoReg
	//wdata
	assign wdata = PCtoReg ? res3 : (MemtoReg ? Read_data_tmp : (Upper ? {ALUout[15:0],16'd0} : ALUout));

	//Wreg_strb
	assign Wreg_strb = (AccWay==2'b11) ? {LSide,LSide ? ~ALUout[1:0] : ALUout[1:0]} : 3'b011;

	//Read_data
	assign Read_data_tmp = LSide ? Read_data_tmp1 : Read_data_tmp2;
	assign ea = ALUout[1:0];

	//LWR
	assign Read_data_tmp1 = (ea==2'b00)   ? MDR : ((ea==2'b01) ? {8'd0,MDR[31:8]} :
				                  ((ea==2'b10)  ? {16'd0,MDR[31:16]} : {24'd0,MDR[31:24]}));

	//LWL&LB&LH
	assign Read_data_tmp2 = (AccWay==2'b11)  ? Read_data_LWL : ((AccWay==2'b01) ? Read_data_LB :
				                  ((AccWay==2'b10) ? Read_data_LH  : MDR));

	assign Read_data_LWL = (ea==2'b00)   ? {24'd0,MDR[7:0]} : ((ea==2'b01) ? {16'd0,MDR[15:0]} :
			                   ((ea==2'b10)  ? {8'd0,MDR[23:0]} : {8'd0,MDR[23:0]}));

	assign Read_data_LB = (ea==2'b00)  ? (Signed ? {{24{MDR[7]}},MDR[7:0]}    : {24'd0,MDR[7:0]}) :
			                 ((ea==2'b01)  ? (Signed ? {{24{MDR[15]}},MDR[15:8]}  : {24'd0,MDR[15:8]}) :
			                 ((ea==2'b10)  ? (Signed ? {{24{MDR[23]}},MDR[23:16]} : {24'd0,MDR[23:16]}) :
				               (Signed       ? {{24{MDR[31]}},MDR[31:24]}           : {24'd0,MDR[31:24]})));

	assign Read_data_LH = (ea==2'b00) ? (Signed ? {{16{MDR[15]}},MDR[15:0]}  : {16'd0,MDR[15:0]}) :
			                  (Signed     ? {{16{MDR[31]}},MDR[31:16]}           : {16'd0,MDR[31:16]});

//MemWrite
	//Write_strb
	assign data_sram_wen = (curr_state==`LD) ? 4'b0000 : Write_strb;
	assign Write_strb = SSide ? Write_strb_tmp1 : Write_strb_tmp2;

	assign Write_strb_tmp1 = (ea==2'b00)   ? 4'b1111 : ((ea==2'b01) ? 4'b1110 :
				                   ((ea==2'b10)  ? 4'b1100 : 4'b1000));

	assign Write_strb_tmp2 = (AccWay==2'b11)   ? Write_strb_SWL : ((AccWay==2'b01) ? Write_strb_SB :
				                   ((AccWay==2'b10)  ? Write_strb_SH  : 4'b1111));

	assign Write_strb_SWL = (ea==2'b00)   ? 4'b0001 : ((ea==2'b01) ? 4'b0011 :
				                  ((ea==2'b10)  ? 4'b0111 : 4'b1111));

	assign Write_strb_SB  = (ea==2'b00)   ? 4'b0001 : ((ea==2'b01) ? 4'b0010 :
			                    ((ea==2'b10)  ? 4'b0100 : 4'b1000));

	assign Write_strb_SH  = (ea==2'b00) ? 4'b0011 : 4'b1100;

	//Write_data
	assign data_sram_wdata = Write_data;
	assign Write_data = SSide ? Write_data_tmp1 : Write_data_tmp2;

	assign Write_data_tmp1 = (ea==2'b00)   ? rdata2 : ((ea==2'b01) ? {rdata2[23:0],8'd0} :
				                   ((ea==2'b10)  ? {rdata2[15:0],16'd0}  : {rdata2[7:0],24'd0}));

	assign Write_data_tmp2 = (AccWay==2'b11)   ? Write_data_SWL : ((AccWay==2'b01) ? Write_data_SB :
				                   ((AccWay==2'b10)  ? Write_data_SH  : rdata2));

	assign Write_data_SWL = (ea==2'b00)   ? {24'd0,rdata2[31:24]} : ((ea==2'b01) ? {16'd0,rdata2[31:16]} :
				                  ((ea==2'b10)  ? {8'd0,rdata2[31:8]}   : rdata2));

	assign Write_data_SB  = (ea==2'b00)   ? {24'd0,rdata2[7:0]}      : ((ea==2'b01) ? {16'd0,rdata2[7:0],8'd0} :
			                    ((ea==2'b10)  ? {8'd0,rdata2[7:0],16'd0} : {rdata2[7:0],24'd0}));

	assign Write_data_SH  = (ea==2'b00) ? {16'd0,rdata2[15:0]} : {rdata2[15:0],16'd0};

//Debug
	assign debug_wb_pc = PC;
	assign debug_wb_rf_wen = (curr_state==`WB) ? 4'b1111 : 4'd0;
	assign debug_wb_rf_wnum = (curr_state==`WB) ?  waddr : 5'd0;
	assign debug_wb_rf_wdata = (curr_state==`WB) ? wdata : 32'd0;
//Performance Counter
/*always@(posedge clk)
begin
	if(rst)
		cycle_cnt <= 32'd0;
	else
		cycle_cnt <= cycle_cnt + 32'd1;
end

assign mips_perf_cnt_0 = cycle_cnt;*/
endmodule
