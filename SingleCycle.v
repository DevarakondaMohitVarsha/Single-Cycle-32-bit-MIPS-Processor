`timescale 1ns / 1ps
//////////////////////////////////////////////////////////////////////////////////
//////////////////////////////////////////////////////////////////////////////////

// Module Name:   	Single Cycle MIPS processor

// Programmers Names & IDs:  		  D.Mohit Varsha                2019H1030026G
//												Rahul Parmani                2019H1030021G
//											 NSV Ram Prathap                2019H1030559G 

/// Objective : 	* To design a Single Cycle MIPS based processor.
//						* The processor is capable of 3 types of instructions R-TYPE , I-TYPE and J-TYPE
//						* More information regarding instruction set and micro architecture can be referred from 
//						"Digital Design and Computer Architecture by David Harris and Sara Harris"
//				
// Contributed By : 	D. Mohit Varsha, Rahul Parmani, NSV Ram Prathap
//

//instruction memory


module imem( input  [5:0] A, output [31:0] RD); //which 5 bits of PC?

  reg  [31:0] RAM [63:0];

  initial begin
      $readmemh("memfile.dat",RAM);
  end

  assign RD = RAM[A]; // word aligned

endmodule


//register file 


module regfile(input clk,
							input we3, 
							input [4:0] A1, input [4:0] A2, input [4:0] A3,
							input [31:0] WD3,
					      output [31:0] RD1, output [31:0] RD2);
					
			reg [31:0] rf [31:0];
					
			always @(posedge clk) begin
				if(we3 == 1'b1) begin
						rf[A3] <= WD3;
				end
			end
			
			assign RD1 = (A1!=0)?rf[A1]:0;
			assign RD2 = (A2!=0)?rf[A2]:0;
					
endmodule


//data memory

module dmem(input clk, input WE, input [31:0] A, input [31:0] WD, output [31:0] RD);

		reg [31:0] RAM [63:0];
		
		
		
		assign RD = RAM[A[31:2]];
		
		always@(posedge clk) begin
		
			if(WE==1'b1) begin
					
				RAM[A[31:2]]<=WD;
				
			end
				
		end

endmodule

//adder

module adder(input [31:0] a, input [31:0] b, output [31:0] y);

		assign y = a+b;
	
endmodule

//left shift

module sl2(input [31:0] a, output [31:0] y);

		assign y = {a[29:0],2'b00};

endmodule

//sign extension

module signext(input [15:0] a, output [31:0] y);

		assign y = { {16{a[15]}}, a};

endmodule

//multiplexer 2 to 1

module mux2 #(parameter WIDTH = 8)
				 (input [WIDTH-1 : 0] x0 , input [WIDTH-1 : 0] x1 , 
				  input s,
 				  output [WIDTH-1 : 0] y);
	
		assign y = s ? x1 : x0;

endmodule

module flopr #(parameter WIDTH = 8)
				  (input clk, input reset,
					input      [WIDTH-1 : 0] d,
					output reg [WIDTH-1 : 0] q);
					
		always@(posedge clk, posedge reset) begin
			
			if(reset) q<=0;
			else		 q<=d;
			
		end

endmodule


//alu


module alu(	input [31:0] A, B, 
            input [2:0] F, 
				output reg [31:0] Y, output Zero);
				
	always @ ( * )
		case (F[2:0])
			3'b000: Y <= A & B;
			3'b001: Y <= A | B;
			3'b010: Y <= A + B;
			//3'b011: Y <= 0;  // not used
			3'b011: Y <= A & ~B;
			3'b101: Y <= A + ~B;
			3'b110: Y <= A - B;
			3'b111: Y <= A < B ? 1:0;
			default: Y <= 0; //default to 0, should not happen
		endcase
	
	assign Zero = (Y == 32'b0);
	
endmodule

// control circuit

module controller(input [5:0] op, input [5:0] funct, input zero,
						output memtoreg, output memwrite,
						output pcsrc, output alusrc,
						output regdst, output regwrite,
						output jump,
						output [2:0] alucontrol);

		wire [1:0] aluop;
		wire branch;
		
		maindec md(op,memtoreg,memwrite,branch,alusrc,regdst,regwrite,jump,aluop);
		aludec ad(funct,aluop,alucontrol);
		
		assign pcsrc = branch & zero;
		
		
endmodule

module maindec(input [5:0]op,
				   output memtoreg, output memwrite,
					output branch, output  alusrc,
					output regdst, output regwrite,
					output jump,
					output [1:0] aluop);
					

		reg [8:0] controls;
		assign {regwrite,regdst,alusrc,branch,memwrite,memtoreg,jump,aluop} = controls;
		
		always@(*) begin
			
			case(op)
				6'b000000 : controls <= 9'b110000010;//Rtype
				6'b100011 : controls <= 9'b101001000;//LW
				6'b101011 : controls <= 9'b001010000;//SW
				6'b000100 : controls <= 9'b000100001;//BEQ
				6'b001000 : controls <= 9'b101000000;//ADDI
				6'b000010 : controls <= 9'b000000100;//J
				default   : controls <= 9'bxxxxxxxxx;//illegal
			endcase
			
		end
		

endmodule


module aludec(input [5:0]funct, input [1:0]aluop, output  reg [2:0] alucontrol);
	
	always@(*) begin
		
		case(aluop)
			2'b00 : alucontrol <= 3'b010; //add for lw sw addi
			2'b01 : alucontrol <= 3'b110; //sub for beq
			default: case(funct) //R type
							6'b100000 : alucontrol <= 3'b010; //add
							6'b100010 : alucontrol <= 3'b110; //sub
							6'b100100 : alucontrol <= 3'b000; //and
							6'b100101 : alucontrol <= 3'b001; //or
							6'b101010 : alucontrol <= 3'b111; //slt
							default   : alucontrol <= 3'bxxx; //illegal
						endcase
		endcase
		
	end


endmodule


// Datapath

module datapath(input  clk, reset,
				input  memtoreg, pcsrc,
				input  alusrc, regdst,
				input  regwrite, jump,
				input  [2:0] alucontrol,
				output zero,
				output [31:0] pc,
				input  [31:0] instr,
				output [31:0] aluout, writedata,
				input  [31:0] readdata);
				
		wire [4:0] writereg;
		wire [31:0] pcnext, pcnextbr, pcplus4, pcbranch;
		wire [31:0] signimm, signimmsh;
		wire [31:0] srca, srcb;
		wire [31:0] result;
		
		// next PC logic
		flopr #(32) pcreg(clk, reset, pcnext, pc);
		adder pcadd1(pc, 32'b100, pcplus4);
		sl2 immsh(signimm, signimmsh);
		adder pcadd2(pcplus4, signimmsh, pcbranch);
		mux2 #(32) pcbrmux(pcplus4, pcbranch, pcsrc, pcnextbr);
		mux2 #(32) pcmux(pcnextbr, {pcplus4[31:28], instr[25:0], 2'b00}, jump, pcnext);
		
		// register file logic
		regfile rf(clk, regwrite, instr[25:21], instr[20:16], writereg, result, srca, writedata);
		mux2 #(5) wrmux(instr[20:16], instr[15:11], regdst, writereg);
		mux2 #(32) resmux(aluout, readdata, memtoreg, result);
		signext se(instr[15:0], signimm);
		
		// ALU logic
		mux2 #(32) srcbmux(writedata, signimm, alusrc, srcb);
		alu alu(srca, srcb, alucontrol, aluout, zero);


endmodule



module mips(input  clk, reset,
				output [31:0] pc,
				input  [31:0] instr,
				output memwrite,
				output [31:0] aluout, writedata,
				input  [31:0] readdata);

			wire memtoreg, alusrc, regdst, regwrite, jump, pcsrc, zero;
			wire [2:0] alucontrol;
			
			//module controller(input clk, input reset, 
			//		input [5:0] op, input [5:0] funct, input zero,
			//	   output memtoreg, output memwrite,
			//		output pcsrc, output alusrc,
			//		output regdst, output regwrite,
			//		output jump,
			//		output [2:0] alucontrol);
			
			controller c(instr[31:26], instr[5:0], zero, memtoreg, memwrite, 
						 pcsrc, alusrc, regdst, regwrite, jump, alucontrol);
			
			datapath dp(clk, reset, memtoreg, pcsrc, alusrc, regdst, regwrite,
             			jump, alucontrol, zero, pc, instr, aluout, writedata, readdata);

endmodule



//top module

module top(input  clk, reset,
		   output [31:0] writedata, dataadr,
		   output memwrite);

	wire [31:0] pc, instr, readdata;
	
	// instantiate processor and memories
	
	mips mips(clk, reset, pc, instr, memwrite, dataadr,writedata, readdata);
	
	imem imem(pc[7:2], instr);
	
	dmem dmem(clk, memwrite, dataadr, writedata, readdata);

endmodule



//test bench

module testbench();

	reg clk;
	reg reset;
	wire [31:0] writedata, dataadr;
	wire memwrite;
	
	// instantiate device to be tested
	top dut (clk, reset, writedata, dataadr, memwrite);
	
	// initialize test
	initial
	begin
		reset <= 1; # 22; reset <= 0;
	end
	
	// generate clock to sequence tests
	always
	begin
		clk <= 1; # 5; clk <= 0; # 5;
	end
	
	// check results
	always @(negedge clk)
	begin
		if (memwrite) begin
			if (dataadr===84 & writedata===7) begin
				$display("Simulation succeeded");
				$stop;
				end else if (dataadr !==80) begin
				$display("Simulation failed");
				$stop;
			end
		end
	end

endmodule
