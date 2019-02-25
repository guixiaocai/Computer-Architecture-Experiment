//sram-like to axi bridge

`define init_r 3'b000
`define addr_wait_r 3'b001
`define axi_addr_wait_r 3'b010
`define axi_read_wait 3'b011
`define axi_read_out  3'b100
`define init_w 3'b000
`define addr_wait_w 3'b001
`define axi_addr_wait_w 3'b010
`define axi_write_wait 3'b011
`define axi_write 3'b100
`define axi_write_out 3'b101
`define axi_addr_wait_r_stall 3'b101

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
reg  reg_data_addr_ok_r;
reg  reg_data_addr_ok_w;
reg  reg_data_data_ok_r;
reg  reg_data_data_ok_w;
reg  reg_inst_addr_ok;
reg  reg_inst_data_ok;
reg  reg_arid;
reg  reg_rready;
reg  reg_wvalid;
reg  reg_bready;
reg  [1:0]  reg_wsize;
reg  [31:0] reg_waddr;
wire [1:0]  ea;
reg  [31:0] reg_wdata;
wire [31:0] wdata_t;
reg         reg_arvalid;
reg  [31:0] reg_araddr;
reg  [1:0]  reg_arsize;
reg  [1:0]  reg_awsize;
reg         reg_awvalid;
reg  [31:0] reg_awaddr;
reg  [2:0]  curr_state_r;
reg  [2:0]  curr_state_w;
reg  [2:0]  next_state_r;
reg  [2:0]  next_state_w;
reg  [31:0] reg_inst_addr;
reg  [1:0]  reg_inst_size;
reg  [31:0] reg_data_addr;
reg  [31:0] reg_data_wdata;
reg  [1:0]  reg_data_size;
reg  [31:0] reg_inst_rdata;
reg  [31:0] reg_data_rdata;
reg  [31:0] reg_data_addr_r;
reg  [31:0] reg_data_size_r;
reg  [31:0] reg_data_addr_w;
reg  [31:0] reg_data_size_w;
reg         reg_r_and_w_ok;
wire        r_and_w_ok;
wire        should_RAW;
reg  [31:0] reg_waddr_t;

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
    curr_state_r <= `init_r;
    curr_state_w <= `init_w;
  end
  else
  begin
    curr_state_r <= next_state_r;
    curr_state_w <= next_state_w;
  end
end

assign should_RAW = curr_state_w==`axi_addr_wait_w && (arid==4'd1 && data_addr[31:2]==reg_data_addr_w[31:2] || arid==4'd0 && inst_addr[31:2]==reg_data_addr_w[31:2])
                 || curr_state_w==`axi_write_wait  && (arid==4'd1 && data_addr[31:2]==reg_waddr[31:2]       || arid==4'd0 && inst_addr[31:2]==reg_waddr[31:2])
                 || curr_state_w==`axi_write       && (arid==4'd1 && data_addr[31:2]==reg_waddr_t[31:2] || arid==4'd0 && inst_addr[31:2]==reg_waddr_t[31:2]) && next_state_w!=`axi_write_out;

always@(*)
begin
  case(curr_state_r)
  `init_r:
  begin
    if(inst_req && !inst_wr|| data_req && !data_wr)
      next_state_r = `addr_wait_r;
    else
      next_state_r = `init_r;
  end
  `addr_wait_r:
  begin
    if(should_RAW)
      next_state_r = `axi_addr_wait_r_stall;
    else
      next_state_r = `axi_addr_wait_r;
  end
  `axi_addr_wait_r:
  begin
    if(arready)
      next_state_r = `axi_read_wait;
    else
      next_state_r = `axi_addr_wait_r;
  end
  `axi_read_wait:
  begin
    if(rvalid)
      next_state_r = `axi_read_out;
    else
      next_state_r = `axi_read_wait;
  end
  `axi_read_out:
  begin
    next_state_r = `init_r;
  end
  `axi_addr_wait_r_stall:
  begin
    if(bvalid && bready)
      next_state_r = `axi_addr_wait_r;
    else
      next_state_r = `axi_addr_wait_r_stall;
  end
  default: next_state_r = `init_r;
  endcase
end

always@(posedge clk)
begin
  if(!resetn)
  begin
    reg_rready <= 1'd0;
    reg_arvalid <= 1'd0;
    reg_inst_addr_ok <= 1'd0;
    reg_data_addr_ok_r <= 1'd0;
    reg_inst_data_ok <= 1'd0;
    reg_data_data_ok_r <= 1'd0;
  end
  else
  begin
    case(curr_state_r)
    `init_r:
    begin
      if(next_state_r==`addr_wait_r)
      begin
        if(data_req && !data_wr)
        begin
          reg_arid <= 4'd1;
          reg_data_addr_ok_r <= 1'd1;
        end
        else
        begin
          reg_arid <= 4'd0;
          reg_inst_addr_ok <= 1'd1;
        end
      end
    end

    `addr_wait_r:
    begin
    if(next_state_r==`axi_addr_wait_r)
    begin
      if(arid==4'd1)
      begin
        reg_data_addr_r <= data_addr;
        reg_data_size_r <= data_size;
        reg_data_addr_ok_r <= 1'd0;
        reg_arvalid <= 1'd1;
      end
      if(arid==4'd0)
      begin
        reg_inst_addr <= inst_addr;
        reg_inst_size <= inst_size;
        reg_inst_addr_ok <= 1'd0;
        reg_arvalid <= 1'd1;
      end
      end
      else if(next_state_r==`axi_addr_wait_r_stall)
      begin
        if(arid==4'd1)
        begin
          reg_data_addr_r <= data_addr;
          reg_data_size_r <= data_size;
          reg_data_addr_ok_r <= 1'd0;
        end
        if(arid==4'd0)
        begin
          reg_inst_addr <= inst_addr;
          reg_inst_size <= inst_size;
          reg_inst_addr_ok <= 1'd0;
        end
      end
    end

    `axi_addr_wait_r:
    begin
      if(next_state_r==`axi_read_wait)
      begin
        reg_rready <= 1'd1;
        reg_arvalid <= 1'd0;
      end
    end
    
    `axi_addr_wait_r_stall:
    begin
      if(next_state_r==`axi_addr_wait_r)
      begin
         reg_arvalid <= 1'd1;
      end
    end

    `axi_read_wait:
    begin
      if(next_state_r==`axi_read_out)
      begin
        reg_rready <= 1'd0;
        if(arid==4'd1)
        begin
          reg_data_rdata <= rdata;
          reg_data_data_ok_r <= 1'd1;
        end
        if(arid==4'd0)
        begin
          reg_inst_rdata <= rdata;
          reg_inst_data_ok <= 1'd1;
        end
      end
    end

    `axi_read_out:
    begin
      reg_data_data_ok_r <= 1'd0;
      reg_inst_data_ok <= 1'd0;
    end

    default:;
    endcase
  end
end

always@(*)
begin
  case(curr_state_w)
  `init_w:
  begin
    if(data_req && data_wr)
      next_state_w = `addr_wait_w;
    else
      next_state_w = `init_w;
  end
  `addr_wait_w:
  begin
    next_state_w = `axi_addr_wait_w;
  end
  `axi_addr_wait_w:
  begin
    if(awready)
      next_state_w = `axi_write_wait;
    else
      next_state_w = `axi_addr_wait_w;
  end
  `axi_write_wait:
  begin
    if(wready)
      next_state_w = `axi_write;
    else
      next_state_w = `axi_write_wait;
  end
  `axi_write:
  begin
    if(bvalid)
      next_state_w = `axi_write_out;
    else
      next_state_w = `axi_write;
  end
  `axi_write_out:
  begin
    next_state_w = `init_w;
  end
  default: next_state_w = `init_w;
  endcase
end

always@(posedge clk)
begin
  if(!resetn)
  begin
    reg_awvalid <= 1'd0;
    reg_wvalid <= 1'd0;
    reg_bready <= 1'd0;
    reg_data_addr_ok_w = 1'd0;
    reg_data_data_ok_w <= 1'd0;
    reg_r_and_w_ok <= 1'd0;
  end
  else
  begin
    case(curr_state_w)
    `init_w:
    begin
      if(next_state_w==`addr_wait_w)
      begin
        reg_data_addr_ok_w <= 1'd1;
      end
      reg_r_and_w_ok <= 1'd0;
    end

    `addr_wait_w:
    begin
      if(next_state_w==`axi_addr_wait_w)
      begin
        reg_data_addr_w <= data_addr;
        reg_data_size_w <= data_size;
        reg_data_wdata <= data_wdata;
        reg_data_addr_ok_w <= 1'd0;
        reg_awvalid <= 1'd1;
      end
    end

    `axi_addr_wait_w:
    begin
      if(next_state_w==`axi_write_wait)
      begin
        reg_wvalid <= 1'd1;
        reg_awvalid <= 1'd0;
        reg_wdata <= reg_data_wdata;
        reg_wsize <= reg_data_size_w;
        reg_waddr <= reg_data_addr_w;
      end
    end

    `axi_write_wait:
    begin
      if(next_state_w==`axi_write)
      begin
        reg_wvalid <= 1'd0;
        reg_bready <= 1'd1;
        reg_waddr_t <= reg_waddr;
      end
    end

    `axi_write:
    begin
      if(next_state_w==`axi_write_out)
      begin
        reg_bready <= 1'd0;
        reg_data_data_ok_w <= 1'd1;
      end
    end

    `axi_write_out:
    begin
      reg_data_data_ok_w <= 1'd0;
      if(r_and_w_ok)
        reg_r_and_w_ok <= 1'd1;
    end

    default:;
    endcase
  end
end

assign arvalid        = reg_arvalid;
assign arid           = reg_arid;
assign araddr         = curr_state_r==`axi_addr_wait_r ? {32{arid==4'd0}} & reg_inst_addr | {32{arid==4'd1}} & reg_data_addr_r : 32'd0;
assign inst_addr_ok   = reg_inst_addr_ok;
assign data_addr_ok_r = reg_data_addr_ok_r;
assign arsize         = curr_state_r==`axi_addr_wait_r ? {32{arid==4'd0}} & reg_inst_size | {32{arid==4'd1}} & reg_data_size_r : 2'd0;

assign rready         = reg_rready;
assign inst_data_ok   = reg_inst_data_ok;
assign data_data_ok_r = reg_data_data_ok_r;
assign inst_rdata     = reg_inst_rdata;
assign data_rdata     = reg_data_rdata;
                      /*{32{rid==4'd1}}
                      & ({32{data_size==2'd0}}
                      & ({32{ea==2'd0}} & {24'd0,rdata[7:0]}        | {32{ea==2'd1}} & {16'd0,rdata[15:8],8'd0}
                      | {32{ea==2'd2}}  & {8'd0,rdata[23:16],16'd0} | {32{ea==2'd3}} & {rdata[31:24],24'd0})
                      | {32{data_size==2'd1}}
                      & ({32{ea==2'd0}} & {16'd0,rdata[15:0] } | {32{ea==2'd2}} & {rdata[31:16],16'd0})
                      | {32{data_size==2'd2}}
                      & {32{ea==2'd0}}  & rdata)*/
                      
assign wid            = 4'd1;
assign awid           = 4'd1;
assign awsize         = reg_data_size_w;
assign awvalid        = reg_awvalid;
assign awaddr         = curr_state_w==`axi_addr_wait_w ? reg_data_addr_w : 32'd0;
assign data_addr_ok_w = reg_data_addr_ok_w;
assign ea = reg_waddr[1:0];

assign wvalid = reg_wvalid;
assign wdata  = reg_wdata;
/*assign wdata_t = ({32{data_size==2'd0}}
              & ({32{ea==2'd0}} & {24'd0,data_wdata[7:0]}       | {32{ea==2'd1}} & {16'd0,data_wdata[15:8],8'd0}
              | {32{ea==2'd2}} & {8'd0,data_wdata[23:16],16'd0} | {32{ea==2'd3}} & {data_wdata[31:24],24'd0})
              | {32{data_size==2'd1}}
              & ({32{ea==2'd0}} & {16'd0,data_wdata[15:0]} | {32{ea==2'd2}} & {data_wdata[31:16],16'd0})
              | {32{data_size==2'd2}}
              & {32{ea==2'd0}}  & data_wdata);*/

assign wstrb  = ({4{reg_wsize==2'd0}}
              & ({4{ea==2'd0}} & 4'd1 | {4{ea==2'd1}} & 4'd2
              | {4{ea==2'd2}}  & 4'd4 | {4{ea==2'd3}} & 4'd8)
              | {4{reg_wsize==2'd1}}
              & ({4{ea==2'd0}} & 4'd3 | {4{ea==2'd2}} & 4'd12)
              | {4{reg_wsize==2'd2}}
              & {4{ea==2'd0}} & 4'd15);

assign bready         = reg_bready;
assign data_data_ok_w = reg_data_data_ok_w;

assign data_data_ok   = data_data_ok_r | data_data_ok_w | reg_r_and_w_ok;
assign data_addr_ok   = data_addr_ok_r | data_addr_ok_w;
assign r_and_w_ok = data_data_ok_r & data_data_ok_w;

endmodule
