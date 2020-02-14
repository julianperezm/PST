with Lower_Layer_UDP;
with Ada.Text_IO;
with Ada.Strings.Unbounded;

package Client_Handler is
	package LLU renames Lower_Layer_UDP;
	package ATIO renames Ada.Text_IO;
	package ASU renames Ada.Strings.Unbounded;
	
	procedure Chat_Handler(From: in LLU.End_Point_Type;
							 To: in LLU.End_Point_Type;
							 P_Buffer: access LLU.Buffer_Type);
end Client_Handler;

