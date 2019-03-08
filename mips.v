`timescale 1ns / 1ps

// main module of single-cycle 32-bit MIPS processor
module mips_MAIN_MODULE(input         clk, reset,
            output [31:0] pc,
            input  [31:0] instr,
            output        memwrite,
            output [31:0] aluout, writedata,
            input  [31:0] readdata);

  wire        branch, memtoreg,
              pcsrc, zero, spra,
              alusrc, regwrite, spregwrite, jump, jal, jumpreg, readhilo;
  wire [1:0]  regdst;
  wire [3:0]  alucontrol;

  controller c(instr[31:26], instr[5:0], instr[10:6], zero,
               memwrite, pcsrc,
               alusrc, regwrite, spregwrite, regdst, memtoreg, jump, jal, jumpreg,
               alucontrol, spra, readhilo);
  datapath dp(clk, reset, memtoreg, pcsrc,
              alusrc, regdst, regwrite, spregwrite, jump, jal, jumpreg, instr[10:6],
              alucontrol,
              zero, pc, instr,
              aluout, writedata, readdata, spra, readhilo);
endmodule

//control module of the processor
module controller(input  [5:0] op, funct,
				  input  [4:0] shamt,
                  input        zero,
                  output       memwrite,
                  output       pcsrc, alusrc,
                  output       regwrite, spregwrite,
                  output [1:0] regdst, 
                  output       memtoreg,
                  output       jump, jal, jumpreg,
                  output [3:0] alucontrol,
                  output       spra, readhilo);

  wire [3:0] aluop;
  wire       branch;

  main_decoder md(op, funct, memwrite, branch,
             alusrc, regwrite, spregwrite, regdst, memtoreg, jump, jal,
             aluop, spra, readhilo);
  alu_decoder  ad(funct, shamt, aluop, alucontrol, jumpreg);

  assign pcsrc = branch & zero;
endmodule

//main decoder
module main_decoder(input  [5:0] op, funct,
               output       memwrite,
               output       branch, alusrc,
               output       regwrite, spregwrite,
               output [1:0] regdst, 
               output       memtoreg,
               output       jump, jal,
               output [3:0] aluop,
               output reg   spra,
               output       readhilo);

  reg [14:0] controls;

  assign {regwrite, regdst, alusrc,
          branch, memwrite,
          memtoreg, jump, jal, aluop, spregwrite, readhilo} = controls;

  always @(*)
    case(op)
      6'b000000: 
      	begin
      		case(funct)
      			6'b011000: controls <= 15'b101000000001010; //mult
      			6'b011010: controls <= 15'b101000000001010; //div
      			default:   
      			  begin
      			    case(funct)
      			      6'b010000: 
      			        begin
      			          spra <= 1'b1;
      			          controls <= 15'b101000000001001;
      			        end
      			      6'b010010: 
      			        begin
      			          spra <= 1'b0;
      			          controls <= 15'b101000000001001;
      			        end
      			      default: controls <= 15'b101000000001000; //other R-type
      			    endcase
      			  end
      		endcase
      	end
      6'b100011: controls <= 15'b100100100000000; //LW
      6'b101011: controls <= 15'b000101000000000; //SW
      6'b000100: controls <= 15'b000010000000100; //BEQ
      6'b001000: controls <= 15'b100100000000000; //ADDI
      6'b000010: controls <= 15'b000000010000000; //J
      6'b001100: controls <= 15'b100100000010000; //ANDI
      6'b001101: controls <= 15'b100100000010100; //ORI
      6'b001111: controls <= 15'b100100000100000; //LUI
      default:   controls <= 15'bxxxxxxxxxxxxxx; //???
    endcase
endmodule

//ALU decoder
module alu_decoder(input      [5:0] funct,
              input      [4:0] shamt,
              input      [3:0] aluop,
              output reg [3:0] alucontrol,
              output     jumpreg);

  always @(*)
    case(aluop)
      4'b0000: alucontrol <= 4'b0010;  // add
      4'b0001: alucontrol <= 4'b0110;  // sub
      4'b0101: alucontrol <= 4'b0001;  // or
      4'b1000: alucontrol <= 4'b1000;  // lui
      default: case(funct)          // RTYPE
          6'b100000: alucontrol <= 4'b0010; // ADD
          6'b100010: alucontrol <= 4'b0110; // SUB
          6'b100101: alucontrol <= 4'b0001; // OR
          6'b000000: alucontrol <= 4'b0011; // SLL
          6'b011000: alucontrol <= 4'b1010; // MULT
          6'b011010: alucontrol <= 4'b1110; // DIV
          default:   alucontrol <= 4'bxxxx; // ???
        endcase
    endcase
    assign jumpreg = (funct == 6'b001000) ? 1 : 0;
endmodule

//datapath module
module datapath(input         clk, reset,
                input         memtoreg, 
                input         pcsrc,
                input         alusrc, 
                input  [1:0]  regdst,
                input         regwrite, spregwrite, jump, jal, jumpreg,
                input  [4:0]  shamt,
                input  [3:0]  alucontrol,
                output        zero,
                output [31:0] pc,
                input  [31:0] instr,
                output [31:0] aluout, writedata,
                input  [31:0] readdata,
                input         spra, readhilo);

  wire [4:0]  writereg;
  wire [31:0] pcnextjr, pcnext, pcnextbr, pcplus4, pcbranch;
  wire [31:0] signimm, signimmsh;
  wire [31:0] srca, srcb, wd0, wd1, sprd;
  wire [31:0] result, resultjal, resulthilo;
  
  ff_resetable #(32) pcreg(clk, reset, pcnext, pc);
  
  adder       pc_add_one(pc, 32'b100, pcplus4);
  
  shift_left_2         immsh(signimm, signimmsh);
  
  adder       pc_add_two(pcplus4, signimmsh, pcbranch);
  
  mux2 #(32)  pc_branch_mux(pcplus4, pcbranch, pcsrc,
                      pcnextbr);
  mux2 #(32)  pc_mux(pcnextbr, {pcplus4[31:28], 
                    instr[25:0], 2'b00}, 
                    jump, pcnext);
  mux2 #(32)  pc_mux_jr(pcnext, srca, 
                    jumpreg, pcnextjr);

  register_file     regFile(clk, regwrite, instr[25:21],
                 instr[20:16], writereg,
                 resulthilo, srca, writedata);
  mux3 #(5)   wr_mux(instr[20:16], instr[15:11], 5'b11111,
                    regdst, writereg);
  mux2 #(32)  result_mux(aluout, readdata,
                     memtoreg, result);
  mux2 #(32)  wr_mux_jal(result, pcplus4, jal,
                      resultjal); //for jal, not usefull in this assignment
  mux2 #(32)  wr_mux_hilo(resultjal, sprd, readhilo, resulthilo); //used in mfhi mflo
  
  signext     se(instr[15:0], signimm);

  mux2 #(32)  srcbmux(writedata, signimm, alusrc,
                      srcb);
  alu         alu(srca, srcb, shamt, alucontrol,
                  aluout, wd0, wd1, zero);

  special_registerfile   sprf(clk, spregwrite, spra, wd0, wd1, sprd); //used in mfhi mflo
endmodule

module alu(input      [31:0] a, b, 
           input      [4:0]  shamt,
           input      [3:0]  alucont, 
           output reg [31:0] result, wd0, wd1,
           output            zero);

  wire [31:0] b2, sum, slt, sra_sign, sra_aux;
  wire [63:0] product, quotient, remainder;
 
  assign b2 = alucont[2] ? ~b:b; 
  assign sum = a + b2 + alucont[2];
  assign slt = sum[31];
  assign sra_sign = 32'b1111_1111_1111_1111 << (32 - shamt);
  assign sra_aux = b >> shamt;
  assign product = a * b;
  assign quotient = a / b;
  assign remainder = a % b;

  always@(*)
    case(alucont[3:0])
      4'b0000: result <= a & b;
      4'b0001: result <= a | b;
      4'b0010: result <= sum;
      4'b0011: result <= b << shamt;
      4'b1011: result <= b << a;
      4'b0100: result <= b >> shamt;
      4'b1100: result <= b >> a;
      4'b0101: result <= sra_sign | sra_aux;
      4'b0110: result <= sum;
      4'b1010: 
        begin
          result <= product[31:0]; 
          wd0    <= product[31:0];
          wd1    <= product[63:32];
        end
      4'b1110: 
        begin
          result <= quotient; 
          wd0    <= quotient;
          wd1    <= remainder;
        end
      4'b1000: result <= b << 5'd16;
    endcase

  assign zero = (result == 32'd0);
endmodule

module register_file(input         clk, 
               input         we3, 
               input  [4:0]  ra1, ra2, wa3, 
               input  [31:0] wd3, 
               output [31:0] rd1, rd2);

  reg [31:0] rf[31:0];

  always @(posedge clk)
    if (we3) rf[wa3] <= wd3;	

  assign rd1 = (ra1 != 0) ? rf[ra1] : 0;
  assign rd2 = (ra2 != 0) ? rf[ra2] : 0;
endmodule

module special_registerfile(input       clk, 
               input         we, 
               input         ra, 
               input  [31:0] wd0, wd1, 
               output [31:0] rd);

  reg [31:0] rf[1:0];

  always @(posedge clk)
    if (we == 1'b1)
      begin
        rf[1'b0] <= wd0;
        rf[1'b1] <= wd1;
      end
   assign rd = (ra != 1'b0) ? rf[1'b1] : rf[1'b0];
endmodule

//adder
module adder(input [31:0] a, b,
             output [31:0] y);

  assign y = a + b;
endmodule

//shift left by 2 bits
module shift_left_2(input  [31:0] a,
           output [31:0] y);

  // shift left by 2
  assign y = {a[29:0], 2'b00};
endmodule

//sign extend
module signext(input  [15:0] a,
               output [31:0] y);
              
  assign y = {{16{a[15]}}, a};
endmodule

//resetable flip flop 
module ff_resetable #(parameter WIDTH = 8)
              (input                  clk, reset,
               input      [WIDTH-1:0] d, 
               output reg [WIDTH-1:0] q);

  always @(posedge clk, posedge reset)
    if (reset) q <= 0;
    else       q <= d;
endmodule

module mux2 #(parameter WIDTH = 8)
             (input  [WIDTH-1:0] d0, d1, 
              input              s, 
              output [WIDTH-1:0] y);

  assign y = s ? d1 : d0; 
endmodule

module mux3 #(parameter WIDTH = 8)
             (input  [WIDTH-1:0] d0, d1, d2,
              input  [1:0]            s, 
              output [WIDTH-1:0] y);

  assign y = (s == 2'b00) ? d0 : ((s == 2'b01) ? d1 : d2); 
endmodule

module top_module(input         clk, reset, 
           output [31:0] writedata, dataadr, 
           output        memwrite);

  wire [31:0] pc, instr, readdata;
  
  // instantiating processor memories
  mips_MAIN_MODULE mips_MAIN_MODULE(clk, reset, pc, instr, memwrite, dataadr, writedata, readdata);
  instruction_memory instruction_memory(pc[7:2], instr);
  data_memory data_memory(clk, memwrite, dataadr, writedata, readdata);

endmodule

module data_memory(input         clk, we,
            input  [31:0] a, wd,
            output [31:0] rd);

  reg  [31:0] RAM[63:0];

  assign rd = RAM[a[31:2]]; 

  always @(posedge clk)
    if (we)
      RAM[a[31:2]] <= wd;
endmodule

module instruction_memory(input  [5:0] a,
            output [31:0] rd);

  reg  [31:0] RAM[63:0];

  initial
    begin
      $readmemh("memfile.dat",RAM);
    end

  assign rd = RAM[a]; 
endmodule


module testbench();

  reg         clk;
  reg         reset;

  wire [31:0] writedata, dataadr;
  wire memwrite;

  // instantiate 
  top_module dut(clk, reset, writedata, dataadr, memwrite);
  

  initial
    begin
      reset <= 1; # 22; reset <= 0;
    end

  // generating clock
  always
    begin
      clk <= 1; # 5; clk <= 0; # 5;
    end
	
endmodule
`timescale 1ns/1ps
module testbench_ALU();
  reg [31:0] a,b;
  reg [4:0] shamt;
  reg [3:0] alucont;
  wire [31:0] result, wd0, wd1;
  wire zero;
  alu test_unit1(
                a,b,
				shamt,
				alucont,
				result,wd0,wd1,
				zero
                );
	initial 
	   begin
	   a = 32'h00000002;
	   b = 32'h00000004;  //add
	   alucont = 4'b0010;
	   #100
	   
	   a = 32'h00000007;
	   b = 32'h00000009;  //sub
	   alucont = 4'b0110;
	   #100
	   
	   a = 32'h00000009;
	   b = 32'h00000009;
	   alucont = 4'b1010;   //mult
	   #100
	   
	   b = 32'h00000007;     //shift,sll
	   shamt = 32'h00000002;
	   alucont = 4'b0011;
	   #100
	   
	   a = 32'h00000008;
	   b = 32'h00000003;
	   alucont = 4'b1110;   //div
	   #100
	   
	   a = 32'h00000008;
	   b = 32'h00000005;        //or
	   alucont = 4'b0001;
	   #100
	   $stop;
	   end
endmodule

module testbench_regFile();
  reg clk;
  reg we3;
  reg [4:0] ra1,ra2,wa3;
  reg [31:0] wd3;
  
  wire[31:0] rd1,rd2;
  register_file test_unit2(
                          .clk(clk),
						  .we3(we3),
						  .ra1(ra1),
						  .ra2(ra2),
						  .wa3(wa3),
						  .wd3(wd3),
						  .rd1(rd1),
						  .rd2(rd2)
						  );
	
	initial
	   begin
	    clk = 1'b0;
		we3 = 1'b0;
		ra1 = 5'b00000;
		ra2 = 5'b00000;
		wa3 = 5'b00000;
		wd3 = 32'h00000000;
		#50
		
		we3 = 1'b1;
		wa3 = 5'b00011;
		wd3 = 32'h00000007;
		#20
		
		we3 = 1'b1;
		wa3 = 5'b00100;
		wd3 = 32'h00000008;
		#20
		
		we3 = 1'b0;
		ra1 = 5'b00011;
		ra2 = 5'b0;
		wd3 = 32'h0;
		#20
		
		we3 = 1'b0;
		ra1 = 5'b0;
		ra2 = 5'b00100;
		wd3 = 32'h0;
		#20
		
		we3 = 1'b0;
		ra1 = 5'b00011;
		ra2 = 5'b00100;
		wd3 = 32'h0;
		#20
        $stop;
	   end
	 always
       begin
       #10
	   clk = ~clk;
      end
   
endmodule
`timescale 1ns/1ps
module testbench_instructionMEM();
  reg [5:0] a;
  wire [31:0] rd;
  instruction_memory test_unit3(
                     .a(a),
					 .rd(rd)
					 );
	initial
	   begin
	   a = 5'b0;
	   #20
	   
	   a = 5'b00001;
	   #20
	   
	   a = 5'b00010;
	   #20
	   
	   a = 5'b00011;
	   #20
	   $stop;
	   end
endmodule
module testbench_dataMEM();
 reg clk;
 reg we;
 reg [31:0] a,wd;
 wire [31:0] rd;
 data_memory test_unit4(
                       .clk(clk),
					   .we(we),
					   .a(a),
					   .wd(wd),
					   .rd(rd)
					   );
	initial
	    begin
		clk = 1'b0;
		
		we = 1'b1;
		a = 32'h0000001c;
		wd = 32'h00000009;
		#20
		
		we = 1'b0;
		a = 32'h0000001c;
		$stop;
		end
    always
        begin
		#10
		clk = ~clk;
        end		
endmodule
`timescale 1ns/1ps
module testbench_signExtend();
reg [15:0] a;
wire [31:0] y;
signext test_unit5(
                   .a(a),
				   .y(y)
				   );

  initial
     begin
	   a = -16'h0003;
	   #20
	   a = 16'h0007;
	   #20
	   a = 16'h0009;
	   #20
	   $stop;
	 end
endmodule
`timescale 1ns/1ps
module testbench_shiftLeft2();
reg [31:0] a;
wire [31:0] y;
  shift_left_2 test_unit6(
                          .a(a),
						   .y(y)
						   );

  initial
     begin
	 a = 32'h00000004;
	 #20
	 a = 32'h00000008;
	 #20
	 a = 32'h00000003;
	 #20
	 $stop;
	 end
endmodule

module testbench_pc();
 reg [31:0] a,b;
 wire [31:0] y;
 adder test_unit7(
                  .a(a),
				  .b(b),
				  .y(y)
				  );
	initial
	  begin
	     a = 32'h00000002;
		 b = 32'h00000004; //pc increment by 4 
		 #20
		 
		 a = 32'h00000006;
		 b = 32'h00000004; //increment by 4
		 #20
		 $stop;
	  end
endmodule

module testbench_pcBranch();
 reg [31:0] a,b;
 wire [31:0] y;
 adder test_unit8(
                  .a(a),
				  .b(b),
				  .y(y)
				  );
	initial
	  begin
	     a = 32'h0000001c; //pcplus4 value here
		 b = 32'h00001c04; // branch target
		 #20
		 
		 a = 32'h00001c20; //pclus4 value here
		 b = 32'h0000d001; //branch target
		 #20
		 $stop;
	  end
endmodule

