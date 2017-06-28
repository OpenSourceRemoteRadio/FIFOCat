# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# %  
# % Copyright (c) 2017, BigCat Wireless Pvt Ltd
# % All rights reserved.
# % 
# % Redistribution and use in source and binary forms, with or without
# % modification, are permitted provided that the following conditions are met:
# % 
# %     * Redistributions of source code must retain the above copyright notice,
# %       this list of conditions and the following disclaimer.
# %
# %     * Redistributions in binary form must reproduce the above copyright
# %       notice, this list of conditions and the following disclaimer in the
# %       documentation and/or other materials provided with the distribution.
# %
# %     * Neither the name of the copyright holder nor the names of its contributors
# %       may be used to endorse or promote products derived from this software
# %       without specific prior written permission.
# % 
# % 
# % 
# % THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
# % AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
# % IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
# % DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE
# % FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
# % DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
# % SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
# % CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
# % OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
# % OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
# % 
#  %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


# Parses file line by line and replaces the device, fifo_depth and bit_width
#!/bin/sh
#exec tclsh "$0" ${1+"$@"}


proc params_find_replace {file_in file_out device fifo_depth bit_width} {

#puts $file_in
#puts $file_out

   set in [open $file_in r]
   set out [open $file_out w]

   set lpm_widthu [ expr {int(log($fifo_depth)/[expr log(2)])} ]
   #puts "lpm_widthu-$lpm_widthu"

   while {[gets $in line] != -1} {
      #puts $line
      if { [string match "intended_device_family=*" $line] } {
         set line "intended_device_family=\"$device\""		
      }
      if { [string match "lpm_numwords=*" $line] } {
         set line "lpm_numwords=\"$fifo_depth\""		
      }
      if { [string match "lpm_width=*" $line] } {
         set line "lpm_width=\"$bit_width\""	 
      }
      if { [string match "lpm_widthu=*" $line] } {
         set line "lpm_widthu=\"$lpm_widthu\""	 
      }
      if { [string match "almost_full_value=*" $line] } {
         set line "almost_full_value=\"[expr $fifo_depth-5]\""		
      }

      puts $out $line
   }
   close $in
   close $out
}

# Create a file cat_fifo_<x>k_to_<y>k_wrap.v
proc create_cat_fifo_wrap { file_in file_out fifo_depth cfifo_depth bit_width } {
#	puts $file_in
#	puts $file_out
	
	# Copy header template
	file copy -force $file_in $file_out

	set fd [open $file_out a]

	# Module Initialization 
	puts $fd "\n\n"
	
	if {$fifo_depth<1024} {
   	   set cat_fifo_out [format {cat_fifo_%s_to_%s_wrap.v} $fifo_depth $cfifo_depth]
   	} elseif {$fifo_depth>=1024 && $cfifo_depth<1024} {
   	   set cat_fifo_out [format {cat_fifo_%sk_to_%s_wrap.v} [expr $fifo_depth/1024] $cfifo_depth]
   	} else {
   	   set cat_fifo_out [format {cat_fifo_%sk_to_%sk_wrap.v} [expr $fifo_depth/1024] [expr $cfifo_depth/1024]]
   	}
	    
        puts $fd [format {     input wire [%s:0] data,  //  buff_input.datain} [expr $bit_width-1]]
        puts $fd "     input  wire        wrreq, //            .wrreq"
        puts $fd "     input  wire        rdreq, //            .rdreq"
        puts $fd "     input  wire        clock, //            .clk"
	puts $fd "     input  wire        aclr,  //            .aclr"
        puts $fd [format {     output  wire [%s:0] q,    //   buff_output.dataout} [expr $bit_width-1]]
        puts $fd [format {     output  reg [%s:0] usedw, //    .usedw} [ expr {int(log($fifo_depth)/[expr log(2)])-1} ]]
        puts $fd "     output  wire        full, //            .full"
        puts $fd "     output  wire        empty //            .empty"
	puts $fd "   );"
	puts $fd "\n\n"

	# Module Declaration
	set no_of_cat_fifo [expr $fifo_depth/$cfifo_depth]

	for {set i 1} {$i <= $no_of_cat_fifo} {incr i} {
    	   puts $fd [format {wire  rd_req_stage_%s ;} $i]
	}
	puts $fd "\n"

	for {set i 1} {$i <= $no_of_cat_fifo} {incr i} {
    	   puts $fd [format {wire  wr_req_stage_%s ;} $i]
	}
	puts $fd "\n"

	for {set i 1} {$i <= $no_of_cat_fifo} {incr i} {
    	   puts $fd [format {wire  [%s:0] rd_data_stage_%s ;}  [expr $bit_width-1] $i]; # Var_Change
	}
	puts $fd "\n"

	for {set i 1} {$i <= $no_of_cat_fifo} {incr i} {
    	   puts $fd [format {wire  [%s:0] wr_data_stage_%s ;}  [expr $bit_width-1] $i]; 
	}
	puts $fd "\n"

	for {set i 1} {$i <= $no_of_cat_fifo} {incr i} {
    	   puts $fd [format {wire  almost_empty_stage_%s ;} $i]; # Var_Change
	}
	puts $fd "\n"

	for {set i 1} {$i <= $no_of_cat_fifo} {incr i} {
    	   puts $fd [format {wire  almost_full_stage_%s ;} $i]; # Var_Change
	}
	puts $fd "\n"

	puts $fd "wire  wr_stage1_full ;"
        puts $fd "wire  final_stage_empty ;"
	puts $fd "\n"

	# Module Assignments
	puts $fd "assign  wr_data_stage_1 = data ;"
	for {set i 2} {$i <= $no_of_cat_fifo} {incr i} {
    	   puts $fd [format {assign  wr_data_stage_%s = rd_data_stage_%s ;} $i [expr $i-1]]; # Var_Change
	}
	puts $fd "\n"

	puts $fd "assign  wr_req_stage_1 = wrreq ;"
	for {set i 2} {$i <= $no_of_cat_fifo} {incr i} {
    	   puts $fd [format {assign  wr_req_stage_%s = rd_req_stage_%s ;} $i [expr $i-1]]; # Var_Change
	}
	puts $fd "\n"
	puts $fd "reg  usedw_incr ;"
	puts $fd "reg  usedw_decr ;"
	puts $fd "\n"

	puts $fd "always @ (posedge clock or posedge aclr)"
	puts $fd "begin"
        puts $fd "  if (aclr) begin"
	puts $fd "    usedw_incr <= 1'b0;"
	puts $fd "  end else if ((rdreq) && !(wrreq) && (usedw == 1)) begin"
	puts $fd "    usedw_incr <= 1'b0;"
	puts $fd "  end else if ((wrreq) && !(rdreq) && (usedw >= 0)) begin"
	puts $fd "    usedw_incr <= 1'b1;"
	puts $fd "  end else begin"
	puts $fd "    usedw_incr <= 1'b0;"
        puts $fd "  end"	
	puts $fd "end"
	puts $fd "\n"

	puts $fd "always @ (posedge clock or posedge aclr)"
	puts $fd "begin"
        puts $fd "  if (aclr) begin"
	puts $fd "    usedw_decr <= 1'b0;"
	puts $fd [format {  end else if ((rdreq) && !(wrreq) && (usedw <= %s)) begin} [expr $fifo_depth-1]];
	puts $fd "    usedw_decr <= 1'b1;"
	puts $fd [format {  end else if ((wrreq) && !(rdreq) && (usedw == %s)) begin} [expr $fifo_depth-2]];
	puts $fd "    usedw_decr <= 1'b0;"
	puts $fd "  end else begin"
	puts $fd "    usedw_decr <= 1'b0;"
        puts $fd "  end"	
	puts $fd "end"
	puts $fd "\n"

	puts $fd "always @ (posedge clock or posedge aclr)"
	puts $fd "begin : STATUS_COUNTER"
        puts $fd "  if (aclr) begin"
	puts $fd "    usedw <= 0;"
  	puts $fd "  // Read but no write."
	puts $fd "  end else if ((rdreq) && !(wrreq) && (usedw_decr)) begin"
	puts $fd "    usedw <= usedw - 1;"
        puts $fd "  // Write but no read."
	puts $fd "  end else if ((wrreq) && !(rdreq) && (usedw_incr)) begin"
	puts $fd "    usedw <= usedw + 1;"
        puts $fd "  end"	
	puts $fd "end"
	puts $fd "\n"

	# Cascaded FIFO Instantiation
	for {set i 1} {$i <= $no_of_cat_fifo} {incr i} {
    	   puts $fd [format {single_cat_fifo_wrap  u_single_fifo_wrap_%s (} $i];
	   puts $fd [format {   .data                      (wr_data_stage_%s),      //  fifo_input.datain} $i]; 
	   puts $fd [format {   .wrreq                     (wr_req_stage_%s),       //            .wrreq} $i]; 
	   puts $fd [format {   .rdreq                     (rd_req_stage_%s),       //            .rdreq} $i];
           puts $fd [format {   .clock                     (clock),                //            .clk}];
	   puts $fd [format {   .aclr                      (aclr),                 //            .aclr}];  
	   puts $fd [format {   .q                         (rd_data_stage_%s),      // fifo_output.dataout} $i];
	   puts $fd [format {   .usedw                     (),                     //            .usedw}]; 

	   if {$i==1} {
	      puts $fd "   .full                      (wr_stage1_full),       //            .full"
	   } else {
	      puts $fd "   .full                      (),                     //            .full"
	   }
   	   puts $fd [format {   .almost_full               (almost_full_stage_%s),  //            .almost_full} $i]; 

	   if {$i==$no_of_cat_fifo} {
	      puts $fd "   .empty                      (final_stage_empty),      //            .empty"
	      puts $fd [format {   .almost_empty              (),                      //            .almost_empty}];
	      puts $fd [format {   .almost_full_next_stage    (), }];
	   } else {
	      puts $fd "   .empty                     (),                     //            .empty"
	      puts $fd [format {   .almost_empty              (almost_empty_stage_%s), //            .almost_empty} $i];
	      puts $fd [format {   .almost_full_next_stage    (almost_full_stage_%s), } [expr $i+1]];
	   } 
	   
	   if {$i==1} {
		   puts $fd "   .select_full_almost_full   (1'b1),"
	   	   puts $fd "   .select_empty_almost_empty (1'b0),"
		   puts $fd "   .rd_req_in		      ()"		 
	   } elseif {$i==$no_of_cat_fifo} {
		   puts $fd "   .select_full_almost_full   (1'b0),"
	   	   puts $fd "   .select_empty_almost_empty (1'b1),"
		   puts $fd "   .rd_req_in		      (rdreq)"		 
	   } else {
		   puts $fd "   .select_full_almost_full   (1'b0),"
	   	   puts $fd "   .select_empty_almost_empty (1'b0),"
		   puts $fd "   .rd_req_in		      ()"		 
	   }
   	   
	   puts $fd ");"
	   puts $fd "\n"

	}
	 puts $fd "assign empty = final_stage_empty ;"
	 puts $fd "//assign full  = wr_stage1_full ;"
	 puts $fd "assign full  = almost_full_stage_1 ;"
	 puts $fd [format {assign q     =  rd_data_stage_%s ;} $no_of_cat_fifo];
	 puts $fd "\n"
	 puts $fd "endmodule"
	 puts $fd "\n"

 	 close $fd

}

# Create a file single_cat_fifo_wrap.v
proc create_single_cat_fifo_wrap { file_in file_out fifo_depth cfifo_depth bit_width } {
#	puts $file_in
#	puts $file_out

	# Copy header template
	file copy -force $file_in $file_out

	set fd [open $file_out a]

	puts $fd "\n\n"
	puts $fd "// single_cat_fifo_wrap.v"
	puts $fd "`timescale 1 ps / 1 ps"

	# Module Initialization 
	puts $fd "module single_cat_fifo_wrap (" 
        puts $fd [format {     input wire [%s:0] data,           //  fifo_input.datain} [expr $bit_width-1]]
        puts $fd "     input wire        wrreq, 	       //            .wrreq"
        puts $fd "     output reg        rdreq, 	       //            .rdreq"
        puts $fd "     input wire        clock, 	       //            .clk"
	puts $fd "     input wire        aclr,  	       //            .aclr"
        puts $fd [format {     output reg [%s:0] q,    	       // fifo_output.dataout} [expr $bit_width-1]]
        puts $fd [format {     output wire [%s:0] usedw,         // 		.usedw} [ expr {int(log($cfifo_depth)/[expr log(2)])-1} ]]
        puts $fd "     output wire        full, 	       //            .full"
        puts $fd "     output wire        empty,         //            .empty"
        puts $fd "     output wire        almost_full,   //            .almost_full"
        puts $fd "     output wire        almost_empty,  //            .almost_empty"
        puts $fd "     input  wire        almost_full_next_stage,"
        puts $fd "     input  wire        select_full_almost_full,"
        puts $fd "     input  wire        select_empty_almost_empty,"
        puts $fd "     input  wire        rd_req_in"
	puts $fd "   );"
	puts $fd "\n\n"

	puts $fd "wire  rst_n ;"
        puts $fd "wire  write_req_reg ;"
	puts $fd "reg  wrreq_1d ;"
        puts $fd "reg  wrreq_2d ;"
	puts $fd "reg  rdreq_reg ;"
        puts $fd "reg  rdreq_reg_1d ;"
	puts $fd [format {reg [%s:0] wr_data_1d ;} [expr $bit_width-1]]
	puts $fd [format {reg [%s:0] wr_data_2d ;} [expr $bit_width-1]]
	puts $fd [format {wire [%s:0] read_data ;} [expr $bit_width-1]]
	puts $fd "\n"


	# FIFO Instantiation
	puts $fd "Generate a FIFO called \"single_cat_fifo\" with the parameters as specified in the file out/params.txt"
	puts $fd "single_cat_fifo  u_single_fifo ("
	puts $fd "   .data                      (wr_data_2d),      //  fifo_input.datain"
	puts $fd "   .wrreq                     (wrreq_2d),        //            .wrreq" 
	puts $fd "   .rdreq                     (rdreq_reg1),      //            .rdreq"
        puts $fd "   .clock                     (clock),           //            .clk"
	puts $fd "   .aclr                      (aclr),            //            .aclr"
	puts $fd "   .q                         (read_data),       // fifo_output.dataout"
	puts $fd "   .usedw                     (usedw),           //            .usedw" 
	puts $fd "   .full                      (full),            //            .full"
	puts $fd "   .empty                     (empty),           //            .empty"
	puts $fd "   .almost_full               (almost_full),     // 	    .almost_full"
	puts $fd "   .almost_empty              (almost_empty)    // 	    .almost_empty"
	puts $fd ");"
	puts $fd "\n"

	# Assignments
	puts $fd "//assign write_req_reg  =  (select_full_almost_full == 1'b1) ?  ((wrreq) & (~full)) : ((~almost_full) & (~almost_empty_previous_stage)) ;"
	puts $fd "assign write_req_reg  =  (select_full_almost_full == 1'b1) ?  ((wrreq) & (~full)) : (wrreq) ;"
	puts $fd "assign rdreq_reg1     = (select_empty_almost_empty  == 1'b1) ?  ((rd_req_in) & (~empty)) : rdreq_reg ;"
	puts $fd "assign rst_n          =  ~aclr;"
	puts $fd "\n"

	puts $fd "always @ (posedge clock or negedge rst_n)"
	puts $fd "begin"
        puts $fd "  if (~rst_n) begin"
	puts $fd "    rdreq_reg <= 1'b0;"
	puts $fd "  end else if (select_empty_almost_empty == 1'b1) begin"
	puts $fd "    rdreq_reg <= ((rd_req_in) & (~empty));"
        puts $fd "  end else begin"
	puts $fd "    rdreq_reg <= ((~almost_full_next_stage) & (~almost_empty));"
        puts $fd "  end"	
        puts $fd "end"	
	puts $fd "\n"


	puts $fd "always @ (posedge clock or negedge rst_n)"
	puts $fd "begin"
        puts $fd "  if (~rst_n) begin"
	puts $fd "    wrreq_1d 	   <= 1'b0;"
	puts $fd "    wrreq_2d     <= 1'b0;"
	puts $fd "    wr_data_1d   <= 32'd0;"
	puts $fd "    wr_data_2d   <= 32'd0;"
	puts $fd "    rdreq        <= 1'b0;"
	puts $fd "    rdreq_reg_1d <= 1'b0;"
	puts $fd "    q            <= 32'd0;"
        puts $fd "  end else begin"
	puts $fd "    wrreq_1d     <= write_req_reg;"
	puts $fd "    wrreq_2d     <= wrreq_1d;"
	puts $fd "    wr_data_1d   <= data;"
	puts $fd "    wr_data_2d   <= wr_data_1d;"
	puts $fd "    rdreq_reg_1d <= rdreq_reg;"
	puts $fd "    rdreq        <= rdreq_reg_1d;"
	puts $fd "    q            <= read_data;"
        puts $fd "  end"	
        puts $fd "end"	
	puts $fd "\n"

        puts $fd "endmodule"	
close $fd

}


# --------------------------- Main ----------------------------------

puts "\n\n***********************************************************"
puts "            Synchronous Cascaded FIFO Generation            "
puts "***********************************************************"

set savedDir [pwd]
puts $savedDir
#set temp_path [string map {\\ \/} [lindex $argv 0]]
#set temp_path [string map {\\ \/} savedDir]
set temp_path $savedDir
set temp_out_path [concat $temp_path/out]
set path [concat \"$temp_path\"]
set out_path [concat \"$temp_path/out\"]
#puts [lindex $argv 0]
#puts $temp_path
#puts "\nWorking Directory is $path"
#puts $out_path
#puts $temp_out_path

############################## Device Selection #################################
array set Devices {
   1 "Arria II GZ"
   2 "Arria V"
   3 "Arria V GZ"
   4 "Cyclone IV E"
   5 "Cyclone V"
   6 "MAX V"
   7 "Arria 10"
   8 "Arria II GX"
   9 "Cyclone IV GX"
   10 "Stratix V"
   11 "Stratix IV"
   12 "MAX II"
   13 "MAX 10"
}

puts "\n\n***********************************************************"
puts "              Device Selection from the List               "
puts "***********************************************************"
foreach name [array names Devices] {
    puts "$name -> $Devices($name)"
}
puts "***********************************************************"

while {1} {
   puts "Choose the Target Device listed from 1 to 13:"
   puts " !!! Note:- Validated with Arria 10 device Only"
   set data [gets stdin]
   set Check [expr {[string is integer -strict $data] && ($data >= 1) && ($data <= 13)}]
   #puts $Check
   if {$Check == 1} {
      set dev_selected "$Devices($data)"
      puts "You have chosen the device: \"$dev_selected\""
      break
   }
}

############################## FIFO Depth Selection #################################

array set FIFO_Depth {
   1 4
   2 8
   3 16
   4 32
   5 64
   6 128
   7 256
   8 512
   9 1024
   10 2048
   11 4096
   12 8192
   13 16384
   14 32768
   15 65536
   16 131072
}

puts "\n\n***********************************************************"
puts "             FIFO Depth Selection from the List            "
puts "***********************************************************"
foreach name [array names FIFO_Depth] {
    puts "$name -> $FIFO_Depth($name) words"
}
puts "***********************************************************"

while {1} {
   puts "Choose the FIFO Depth listed from 1 to 16:"
   set data [gets stdin]
   set Check [expr {[string is integer -strict $data] && ($data >= 1) && ($data <= 16)}]
   #puts $Check
   if {$Check == 1} {
      set depth_selected $FIFO_Depth($data)
      puts "You have chosen the depth: $depth_selected words"
      break
   }
}

############################## FIFO Bit width Selection #################################

puts "\n\n***********************************************************"
puts "              FIFO Bit width Selection                     "
puts "***********************************************************"
puts "1. Same bit width is used for FIFO read and write"
puts "2. Bit width should be divisible by 4"
puts "3. Minimum bit width is 4 and maximum is 256"
puts "***********************************************************"

while {1} {
   puts "Enter bit width of the FIFO:"
   set data [gets stdin]
   set Check [expr {[string is integer -strict $data] && ($data >= 4) && ($data <= 256) && ($data%4 == 0)}]
   #puts $Check
   if {$Check == 1} {
      set bits_selected $data
      puts "You have chosen the bit width: $bits_selected"
      break
   }
}


############################## Single/Cascaded FIFO Selection #################################

##puts "\n\n***********************************************************"
##puts "             Single/Cascaded FIFO Selection                "
##puts "***********************************************************"
##puts "1. Single FIFO (If timing is not a constraint)"
##puts "2. Cascaded FIFO"
##puts "***********************************************************"
##
##while {1} {
##   puts "Choose the type of FIFO(1 OR 2):"
##   set data [gets stdin]
##   set Check [expr {[string is integer -strict $data] && ($data >= 1) && ($data <= 2)}]
##   #puts $Check
##   if {$Check == 1} {
##      if {$data == 1} {
##         set fifo_type_selected "Single FIFO"
##	 set cfifo_depth_selected $depth_selected 
##      } else {
##	 set fifo_type_selected "Cascaded FIFO"
##      }
##      puts "You have chosen the type: \"$fifo_type_selected\""
##      break
##   }
##}

set fifo_type_selected "Cascaded FIFO"
if {$fifo_type_selected == "Cascaded FIFO"} {
   while {1} {
      puts "\nEnter the depth of Cascaded FIFO:"
      puts "<Minimum size should not be less than 4 words>"
      puts "<Maximum size should not exceed the total FIFO depth($depth_selected words)>"
      puts "<The total FIFO depth should be divisible by the Cascaded FIFO depth>"
      set data [gets stdin]
      set Check [expr {[string is integer -strict $data] && ($data>=4) && ($data<=$depth_selected) && ($depth_selected%$data==0 )}]
      #set Check [expr {[string is integer -strict $data] && ($data<=$depth_selected) && ($depth_selected%$data==0 )}]
      #puts $Check
      set cfifo_depth_selected $data        
      if {$Check == 1 && $cfifo_depth_selected != $depth_selected } {
         puts "You have chosen the cascaded fifo depth: $cfifo_depth_selected words"
         break
      } elseif {$Check == 1 && $cfifo_depth_selected == $depth_selected } {
         puts "Size of both FIFOs are same."
         puts "Cascaded FIFO can not be generated!."
	 #puts "So generating a single FIFO of depth: $cfifo_depth_selected words"
	 #set fifo_type_selected "Single FIFO"
	 #break
	 return
      } 
   }
}
#puts $temp_out_path
file delete -force $temp_out_path
file mkdir "$temp_out_path/" 

params_find_replace "$temp_path/params_in.txt" "$temp_out_path/params.txt" $dev_selected $cfifo_depth_selected $bits_selected

puts "\n\n***********************************************************"
puts " Generating ..."
puts "***********************************************************"

if {$fifo_type_selected == "Cascaded FIFO"} {
	
   if {$depth_selected<1024} {
      set cat_fifo_out [format {cat_fifo_%s_to_%s_wrap.v} $depth_selected $cfifo_depth_selected]
   } elseif {$depth_selected>=1024 && $cfifo_depth_selected<1024} {
      set cat_fifo_out [format {cat_fifo_%sk_to_%s_wrap.v} [expr $depth_selected/1024] $cfifo_depth_selected]
   } else {
      set cat_fifo_out [format {cat_fifo_%sk_to_%sk_wrap.v} [expr $depth_selected/1024] [expr $cfifo_depth_selected/1024]]
   }
   
   #puts $cat_fifo_out
   create_cat_fifo_wrap "$temp_path/fifo_cat_wrap_temp.v" "$temp_out_path/$cat_fifo_out" $depth_selected $cfifo_depth_selected $bits_selected

   #file copy -force "$temp_path/single_cat_fifo_wrap.v" "$temp_out_path/single_cat_fifo_wrap.v" 
   create_single_cat_fifo_wrap "$temp_path/fifo_cat_wrap_temp.v" "$temp_out_path/single_cat_fifo_wrap.v" $depth_selected $cfifo_depth_selected $bits_selected

}

puts "Required Output Files are:" 
if {$fifo_type_selected == "Cascaded FIFO"} {
	puts "$temp_out_path/$cat_fifo_out"
	puts "$temp_out_path/single_cat_fifo_wrap.v"
}

puts "***********************************************************\n\n"

