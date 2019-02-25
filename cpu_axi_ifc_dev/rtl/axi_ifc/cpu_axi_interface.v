//sram-like to axi bridge
module cpu_axi_interface(
    input        clk,
    input        resetn,

    //inst sram-like
    input         inst_req,
    input         inst_wr,
    input  [1:0]  inst_size,
    input  [31:0] inst_addr,
    input  [31:0] inst_wdata,     //ignore
    output [31:0] inst_rdata,
    output        inst_addr_ok,
    output        inst_data_ok,

    //data sram-like
    input         data_req,
    input         data_wr,
    input  [1:0]  data_size,
    input  [31:0] data_addr,
    input  [31:0] data_wdata,
    output [31:0] data_rdata,
    output        data_addr_ok,
    output        data_data_ok,

    //axi
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
    output        bready
);

wire data_addr_ok_r;
wire data_addr_ok_w;
wire data_data_ok_r;
wire data_data_ok_w;
reg  reg_arid;
reg  reg_rready;
reg  reg_wvalid;
reg  reg_bready;
wire [1:0] ea;

assign arlen   = 8'd0;
assign arburst = 2'd1;
assign arlock  = 2'd0;
assign arcache = 4'd0;
assign arprot  = 3'd0;
assign awid    = 4'd1;
assign awlen   = 8'd0;
assign awburst = 2'd1;
assign awlock  = 2'd0;
assign awcache = 4'd0;
assign awprot  = 3'd0;
assign wid     = 4'd1;
assign wlast   = 1'd1;

always@(posedge clk)
begin
  if(!resetn)
  begin
    reg_arid <= 4'd0;
  end
  else if(data_data_ok || inst_data_ok)
  begin
    reg_arid <= data_req ? 4'd1 : 4'd0;
  end
  end
end

assign arvalid        = inst_req & ~inst_wr | data_req & ~data_wr;
assign arid           = reg_arid;
assign araddr         = {32{arid==4'd0}} & {inst_addr[31:2],2'd0} | {32{arid==4'd1}} & {data_addr[31:2],2'd0};
assign inst_addr_ok   = arvalid && arready && arid==4'd0;
assign data_addr_ok_r = arvalid && arready && arid==4'd1;
assign arsize         = 2'd2;


always@(posedge clk)
begin
  if(!resetn)
  begin
    reg_rready <= 1'd0;
  end
  else if(arready && arvalid)
  begin
    reg_rready <= 1'd1;
  end
  else if(rvalid)
  begin
    reg_rready <= 1'b0;
  end
end

assign ea             = data_addr[1:0];
assign rready         = reg_rready;
assign inst_data_ok   = rid==4'd0 && rready && rvalid;
assign data_data_ok_r = rid==4'd1 && rready && rvalid;
assign inst_rdata     = {32{rid==4'd0}} & rdata;
assign data_rdata     = {32{rid==4'd1}}
                      & ({32{data_size==2'd0}}
                      & ({32{ea==2'd0}} & {24'd0,rdata[7:0]}        | {32{ea==2'd1}} & {16'd0,rdata[15:8],8'd0}
                      | {32{ea==2'd2}}  & {8'd0,rdata[23:16],16'd0} | {32{ea==2'd3}} & {rdata[31:24],24'd0})
                      | {32{data_size==2'd1}}
                      & ({32{ea==2'd0}} & {16'd0,rdata[15:0] } | {32{ea==2'd2}} & {rdata[31:16],16'd0})
                      | {32{data_size==2'd2}}
                      & {32{ea==2'd0}}  & rdata);

assign wid            = 4'd1;
assign awid           = 4'd1;
assign bid            = 4'd1;
assign awsize         = 2'd2;
assign awvalid        = data_req && data_wr;
assign awaddr         = {data_addr[31:0],2'd0};
assign data_addr_ok_w = awvalid && awready;

always@(posedge clk)
begin
  if(!resetn)
  begin
    reg_wvalid <= 1'd0;
  end
  else if(awvalid && awready)
  begin
    reg_wvalid <= 1'd1;
  end
  else if(wready)
  begin
    reg_wvalid <= 1'd0;
  end
end
assign wvalid = reg_wvalid;
assign wdata  = ({32{data_size==2'd0}}
              & ({32{ea==2'd0}} & {24'd0,data_wdata[7:0]}       | {32{ea==2'd1}} & {16'd0,data_wdata[15:8],8'd0}
              | {32{ea==2'd2}} & {8'd0,data_wdata[23:16],16'd0} | {32{ea==2'd3}} & {data_wdata[31:24],24'd0})
              | {32{data_size==2'd1}}
              & ({32{ea==2'd0}} & {16'd0,data_wdata[15:0]} | {32{ea==2'd2}} & {data_wdata[31:16],16'd0})
              | {32{data_size==2'd2}}
              & {32{ea==2'd0}}  & data_wdata);

assign wstrb  = ({4{data_size==2'd0}}
              & ({4{ea==2'd0}} & 4'd1 | {4{ea==2'd1}} & 4'd2
              | {4{ea==2'd2}}  & 4'd4 | {4{ea==2'd3}} & 4'd8)
              | {4{data_size==2'd1}}
              & ({4{ea==2'd0}} & 4'd3 | {4{ea==2'd2}} & 4'd12)
              | {4{data_size==2'd2}}
              & {4{ea==2'd0}} & 4'd15);

always@(posedge clk)
begin
 if(!resetn)
 begin
   reg_bready <= 1'd0;
 end
 else if(wvalid && wready)
 begin
   reg_bready <= 1'd1;
 end
 else if(bvalid)
 begin
   reg_bready <= 1'd0;
 end
end
assign bready         = reg_bready;
assign data_data_ok_w = bready && bvalid;

assign data_data_ok   = data_data_ok_r | data_data_ok_w;
assign data_addr_ok   = data_addr_ok_r | data_addr_ok_w;


endmodule
