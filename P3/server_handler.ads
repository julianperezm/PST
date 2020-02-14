with Ada.Strings.Unbounded;
with Ada.Calendar;
with Lower_Layer_UDP;
with Maps_G;
with Ada.Command_Line;


package Server_Handler is
	package ASU renames Ada.Strings.Unbounded;
	package LLU renames Lower_Layer_UDP;
	package ACL renames Ada.Command_Line;

	
	type Active_Client_Value_Type is record
		Client_EP_Handler: LLU.End_Point_Type;
		Last_Connection: Ada.Calendar.Time;
	end record;
	
	Max_Clients: Natural := Integer'value(ACL.Argument(2));
	
	package Active_Clients_Package is new Maps_G(Key_Type => ASU.Unbounded_String, 
												 Value_Type => Active_Client_Value_Type, 
												 Max_Length => Max_Clients, 
												"=" => ASU."=");
	package ACP renames Active_Clients_Package;
	
	package Inactive_Clients_Package is new Maps_G(Key_Type => ASU.Unbounded_String, 
													Value_Type => Ada.Calendar.Time, 
													Max_Length => 150, 
													"=" => ASU."=");
	package ICP renames Inactive_Clients_Package;
	
	procedure Chat_Handler(From: in LLU.End_Point_Type;
		To: in LLU.End_Point_Type;
		P_Buffer: access LLU.Buffer_Type);
		
	procedure Active_Collection;
	
	procedure Inactive_Collection;
	
end Server_Handler;
