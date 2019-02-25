/*------------------------------------------------------------------------------
--------------------------------------------------------------------------------
Copyright (c) 2016, Loongson Technology Corporation Limited.

All rights reserved.

Redistribution and use in source and binary forms, with or without modification,
are permitted provided that the following conditions are met:

1. Redistributions of source code must retain the above copyright notice, this 
list of conditions and the following disclaimer.

2. Redistributions in binary form must reproduce the above copyright notice, 
this list of conditions and the following disclaimer in the documentation and/or
other materials provided with the distribution.

3. Neither the name of Loongson Technology Corporation Limited nor the names of 
its contributors may be used to endorse or promote products derived from this 
software without specific prior written permission.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND 
ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED 
WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE 
DISCLAIMED. IN NO EVENT SHALL LOONGSON TECHNOLOGY CORPORATION LIMITED BE LIABLE
TO ANY PARTY FOR DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR 
CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE 
GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) 
HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT 
LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF
THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
--------------------------------------------------------------------------------
------------------------------------------------------------------------------*/
`timescale 1ns / 1ps

`define TRACE_REF_FILE "../../../../../../golden_trace.txt"
`define CONFREG_NUM_REG      soc_lite.confreg.num_data
`define CONFREG_OPEN_TRACE   soc_lite.confreg.open_trace
`define CONFREG_NUM_MONITOR  soc_lite.confreg.num_monitor
`define CONFREG_UART_DISPLAY soc_lite.confreg.write_uart_valid
`define CONFREG_UART_DATA    soc_lite.confreg.write_uart_data
`define END_PC 32'hbfc00100

module tb_top( );
reg resetn;
reg clk;

//goio
wire [15:0] led;
wire [1 :0] led_rg0;
wire [1 :0] led_rg1;
wire [7 :0] num_csn;
wire [6 :0] num_a_g;
wire [7 :0] switch;
wire [3 :0] btn_key_col;
wire [3 :0] btn_key_row;
wire [1 :0] btn_step;
assign switch      = 8'd0;
assign btn_key_row = 4'd0;
assign btn_step    = 2'd3;

initial
begin
    clk = 1'b0;
    resetn = 1'b0;
    #2000;
    resetn = 1'b1;
end

/*initial
begin
    #100000;
    force soc_lite.confreg.btn_key_data[15:0] = 16'h0001;
    #1000;
    release soc_lite.confreg.btn_key_data[15:0];
    #5000;
    force soc_lite.confreg.btn_key_data[15:0] = 16'h0002;
    #200000;
    release soc_lite.confreg.btn_key_data[15:0];
    #5000;
    force soc_lite.confreg.btn_key_data[15:0] = 16'h0004;
    #200000;
    release soc_lite.confreg.btn_key_data[15:0];
    #5000;
    force soc_lite.confreg.btn_key_data[15:0] = 16'h0008;
    #200000;
    release soc_lite.confreg.btn_key_data[15:0];
    #5000;
    force soc_lite.confreg.btn_key_data[15:0] = 16'h0001;
    #1000;
    release soc_lite.confreg.btn_key_data[15:0];   
end*/

always #5 clk=~clk;
soc_lite_top #(.SIMULATION(1'b1)) soc_lite
(
       .resetn      (resetn     ), 
       .clk         (clk        ),
    
        //------gpio-------
        .num_csn    (num_csn    ),
        .num_a_g    (num_a_g    ),
        .led        (led        ),
        .led_rg0    (led_rg0    ),
        .led_rg1    (led_rg1    ),
        .switch     (switch     ),
        .btn_key_col(btn_key_col),
        .btn_key_row(btn_key_row),
        .btn_step   (btn_step   )
    );   

//soc lite signals
//"soc_clk" means clk in cpu
//"wb" means write-back stage in pipeline
//"rf" means regfiles in cpu
//"w" in "wen/wnum/wdata" means writing
wire soc_clk;
wire [31:0] debug_wb_pc;
wire [3 :0] debug_wb_rf_wen;
wire [4 :0] debug_wb_rf_wnum;
wire [31:0] debug_wb_rf_wdata;
assign soc_clk           = soc_lite.cpu_clk;
assign debug_wb_pc       = soc_lite.debug_wb_pc;
assign debug_wb_rf_wen   = soc_lite.debug_wb_rf_wen;
assign debug_wb_rf_wnum  = soc_lite.debug_wb_rf_wnum;
assign debug_wb_rf_wdata = soc_lite.debug_wb_rf_wdata;

//wdata[i*8+7 : i*8] is valid, only wehile wen[i] is valid
wire [31:0] debug_wb_rf_wdata_v;
assign debug_wb_rf_wdata_v[31:24] = debug_wb_rf_wdata[31:24] & {8{debug_wb_rf_wen[3]}};
assign debug_wb_rf_wdata_v[23:16] = debug_wb_rf_wdata[23:16] & {8{debug_wb_rf_wen[2]}};
assign debug_wb_rf_wdata_v[15: 8] = debug_wb_rf_wdata[15: 8] & {8{debug_wb_rf_wen[1]}};
assign debug_wb_rf_wdata_v[7 : 0] = debug_wb_rf_wdata[7 : 0] & {8{debug_wb_rf_wen[0]}};

// open the trace file;
integer trace_ref;
initial begin
    trace_ref = $fopen(`TRACE_REF_FILE, "w");
end

reg        debug_end;
// generate trace
always @(posedge soc_clk)
begin
    if(|debug_wb_rf_wen && debug_wb_rf_wnum!=5'd0)
    begin
        $fdisplay(trace_ref, "%h %h %h %h", `CONFREG_OPEN_TRACE,
            debug_wb_pc, debug_wb_rf_wnum, debug_wb_rf_wdata_v);
    end
end

//monitor numeric display
reg [7:0] err_count;
wire [31:0] confreg_num_reg = `CONFREG_NUM_REG;
reg  [31:0] confreg_num_reg_r;
always @(posedge soc_clk)
begin
    confreg_num_reg_r <= confreg_num_reg;
    if (!resetn)
    begin
        err_count <= 8'd0;
    end
    else if (confreg_num_reg_r != confreg_num_reg && `CONFREG_NUM_MONITOR)
    begin
        if(confreg_num_reg[7:0]!=confreg_num_reg_r[7:0]+1'b1)
        begin
            $display("--------------------------------------------------------------");
            $display("[%t] Error(%d)!!! Occurred in number 8'd%02d Functional Test Point!",$time, err_count, confreg_num_reg[31:24]);
            $display("--------------------------------------------------------------");
            err_count <= err_count + 1'b1;
        end
        else if(confreg_num_reg[31:24]!=confreg_num_reg_r[31:24]+1'b1)
        begin
            $display("--------------------------------------------------------------");
            $display("[%t] Error(%d)!!! Unknown, Functional Test Point numbers are unequal!",$time,err_count);
            $display("--------------------------------------------------------------");
            $display("==============================================================");
            err_count <= err_count + 1'b1;
        end
        else
        begin
            $display("----[%t] Number 8'd%02d Functional Test Point PASS!!!", $time, confreg_num_reg[31:24]);
        end
    end
end

//monitor test
initial
begin
    $timeformat(-9,0," ns",10);
    while(!resetn) #5;
    $display("==============================================================");
    $display("Test begin!");

    #10000;
    while(`CONFREG_NUM_MONITOR)
    begin
        #10000;
        $display ("        [%t] Test is running, debug_wb_pc = 0x%8h",$time, debug_wb_pc);
    end
end

//模拟串口打印
wire uart_display;
wire [7:0] uart_data;
assign uart_display = `CONFREG_UART_DISPLAY;
assign uart_data    = `CONFREG_UART_DATA;

always @(posedge soc_clk)
begin
    if(uart_display)
    begin
        if(uart_data==8'hff)
        begin
            $finish;
        end
        else
        begin
            $write("%c",uart_data);
        end
    end
end

//test end
wire test_end = (debug_wb_pc==`END_PC) || (uart_display && uart_data==8'hff);
always @(posedge soc_clk)
begin
    if (!resetn)
    begin
        debug_end <= 1'b0;
    end
    else if(test_end && !debug_end)
    begin
        debug_end <= 1'b1;
	    $display("==============================================================");
	    $display("gettrace end!");
		 #10;
	    $fclose(trace_ref);
        if (err_count!=8'd0)
        begin
            $display("Fail in generating trace file!!! Total %d errors!",err_count);
        end
        else
        begin
            $display("----Succeed in generating trace file!");
        end
	    $finish;
	end
end

wire [31:0] cp0_status;
wire [31:0] cp0_cause;
wire [31:0] cp0_compare;
wire [31:0] cp0_count;
wire [7:0]  cp0_cause_ip;
wire [7:0]  cp0_status_im;
wire        cp0_status_ie;
wire        cp0_status_exl;
wire [5:0]  hard_int;
wire [7:0]  btn_key;

assign cp0_status     = soc_lite.cpu.u_execute.status_value;
assign cp0_cause      = soc_lite.cpu.u_execute.cause_value;
assign cp0_compare    = soc_lite.cpu.u_execute.compare_value;
assign cp0_count      = soc_lite.cpu.u_execute.count_value;
assign cp0_cause_ip   = cp0_cause[15:8];
assign cp0_status_im  = cp0_status[15:8];
assign cp0_status_ie  = cp0_status[0];
assign cp0_status_exl = cp0_status[1];
assign hard_int       = soc_lite.cpu.u_interface.int_n_i;
assign btn_key        = soc_lite.confreg.btn_key_data[7:0];

wire [31:0] t0;
wire [31:0] t1;
wire [31:0] t2;
wire [31:0] t3;
wire [31:0] t4;
wire [31:0] k0;
wire [31:0] k1;
assign t0 = soc_lite.cpu.u_decode.u0_gr_heap.heap_08;
assign t1 = soc_lite.cpu.u_decode.u0_gr_heap.heap_09;
assign t2 = soc_lite.cpu.u_decode.u0_gr_heap.heap_10;
assign t3 = soc_lite.cpu.u_decode.u0_gr_heap.heap_11;
assign t4 = soc_lite.cpu.u_decode.u0_gr_heap.heap_12;
assign k0 = soc_lite.cpu.u_decode.u0_gr_heap.heap_26;
assign k1 = soc_lite.cpu.u_decode.u0_gr_heap.heap_27;

endmodule
