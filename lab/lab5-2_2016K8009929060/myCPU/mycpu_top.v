`timescale 10 ns / 1 ns

module mycpu_top(
	input  [5:0]  int,   //high active
	input         aclk,
	input         aresetn,   //low active

	//ar
	output [3:0]  arid,
	output [31:0] araddr,
	output [7:0]  arlen,          //ignore
	output [2:0]  arsize,
	output [1:0]  arburst,        //ignore
	output [1:0]  arlock,         //ignore
	output [3:0]  arcache,        //ignore
	output [2:0]  arprot,         //ignore
	output        arvalid,
	input         arready,
	//r
	input  [3:0]  rid,
	input  [31:0] rdata,
	input  [1:0]  rresp,          //ignore
	input         rlast,          //ignore
	input         rvalid,
	output        rready,
	//aw
	output [3:0]  awid,
	output [31:0] awaddr,
	output [7:0]  awlen,          //ignore
	output [2:0]  awsize,
	output [1:0]  awburst,        //ignore
	output [1:0]  awlock,         //ignore
	output [3:0]  awcache,        //ignore
	output [2:0]  awprot,         //ignore
	output        awvalid,
	input         awready,
	//w
	output [3:0]  wid,
	output [31:0] wdata,
	output [3:0]  wstrb,
	output        wlast,          //ignore
	output        wvalid,
	input         wready,
	//b
	input  [3:0]  bid,
	input  [1:0]  bresp,          //ignore
	input         bvalid,
	output        bready,

	//debug interface
	output [31:0] debug_wb_pc,
	output [3:0]  debug_wb_rf_wen,
	output [4:0]  debug_wb_rf_wnum,
	output [31:0] debug_wb_rf_wdata
);

  //TODO: Please add your MIPS CPU code here

wire        inst_req;
wire        inst_wr;
wire [1:0]  inst_size;
wire [31:0] inst_addr;
wire [31:0] inst_wdata;     //ignore
wire [31:0] inst_rdata;
wire        inst_addr_ok;
wire        inst_data_ok;
wire        data_req;
wire        data_wr;
wire [1:0]  data_size;
wire [31:0] data_addr;
wire [31:0] data_wdata;
wire [31:0] data_rdata;
wire        data_addr_ok;
wire        data_data_ok;

cpu_inner_core u_cpu_inner_core(
	.clk           (aclk          ),    //in
	.resetn        (aresetn       ),    //in
	.int_t         (int           ),    //in

	.inst_req      (inst_req      ),    //out
	.inst_wr       (inst_wr       ),    //out
	.inst_size     (inst_size     ),    //out
	.inst_addr     (inst_addr     ),    //out
	.inst_wdata    (inst_wdata    ),    //out
	.inst_rdata    (inst_rdata    ),    //in
	.inst_addr_ok  (inst_addr_ok  ),    //in
	.inst_data_ok  (inst_data_ok  ),    //in

	.data_req      (data_req      ),    //out
	.data_wr       (data_wr       ),    //out
	.data_size     (data_size     ),    //out
	.data_addr     (data_addr     ),    //out
	.data_wdata    (data_wdata    ),    //out
	.data_rdata    (data_rdata    ),    //in
	.data_addr_ok  (data_addr_ok  ),    //in
	.data_data_ok  (data_data_ok  ),    //in

	.debug_wb_pc        (debug_wb_pc        ),    //out
	.debug_wb_rf_wen    (debug_wb_rf_wen    ),    //out
	.debug_wb_rf_wnum   (debug_wb_rf_wnum   ),    //out
	.debug_wb_rf_wdata  (debug_wb_rf_wdata  )
	);


cpu_axi_interface u_cpu_axi_interface(
	.clk           (aclk          ),    //in
	.resetn        (aresetn       ),    //in

	.inst_req      (inst_req      ),    //in
	.inst_wr       (inst_wr       ),    //in
	.inst_size     (inst_size     ),    //in
	.inst_addr     (inst_addr     ),    //in
	.inst_wdata    (inst_wdata    ),    //in
	.inst_rdata    (inst_rdata    ),    //out
	.inst_addr_ok  (inst_addr_ok  ),    //out
	.inst_data_ok  (inst_data_ok  ),    //out

	.data_req      (data_req      ),    //in
	.data_wr       (data_wr       ),    //in
	.data_size     (data_size     ),    //in
	.data_addr     (data_addr     ),    //in
	.data_wdata    (data_wdata    ),    //in
	.data_rdata    (data_rdata    ),    //out
	.data_addr_ok  (data_addr_ok  ),    //out
	.data_data_ok  (data_data_ok  ),    //out

	.arid          (arid          ),
	.araddr        (araddr        ),
	.arlen         (arlen         ),
	.arsize        (arsize        ),
	.arburst       (arburst       ),
	.arlock        (arlock        ),
	.arcache       (arcache       ),
	.arprot        (arprot        ),
	.arvalid       (arvalid       ),
	.arready       (arready       ),

	.rid           (rid           ),
	.rdata         (rdata         ),
	.rresp         (rresp         ),
	.rlast         (rlast         ),
	.rvalid        (rvalid        ),
	.rready        (rready        ),

	.awid          (awid          ),
	.awaddr        (awaddr        ),
	.awlen         (awlen         ),
	.awsize        (awsize        ),
	.awburst       (awburst       ),
	.awlock        (awlock        ),
	.awcache       (awcache       ),
	.awprot        (awprot        ),
	.awvalid       (awvalid       ),
	.awready       (awready       ),

	.wid           (wid           ),
	.wdata         (wdata         ),
	.wstrb         (wstrb         ),
	.wlast         (wlast         ),
	.wvalid        (wvalid        ),
	.wready        (wready        ),

	.bid           (bid           ),
	.bresp         (bresp         ),
	.bvalid        (bvalid        ),
	.bready        (bready        )
	);

endmodule
