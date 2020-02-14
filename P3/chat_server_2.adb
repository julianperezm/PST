with Ada.Text_IO;
with Ada.Command_Line;
with Ada.Strings.Unbounded;
with Ada.Calendar;
with Lower_Layer_UDP;
with Chat_Messages;
with Server_Handler;

procedure Chat_Server_2 is
	package ATIO renames Ada.Text_IO;
	package ACL renames Ada.Command_Line;
	package ASU renames Ada.Strings.Unbounded;
	use type ASU.Unbounded_String;
	package LLU renames Lower_Layer_UDP;
	package CM renames Chat_Messages;
	use type CM.Message_Type;
	package SH renames Server_Handler;	
	
	Usage_Error: exception;
	
	procedure Arguments_Control(Port: out Integer; Max_Clients: out Integer) is 
	begin
		if ACL.Argument_Count /= 2 then
			raise Usage_Error;
		end if;

		Port := Integer'Value(ACL.Argument(1));
		Max_Clients := Integer'Value(ACL.Argument(2));
		
		if Max_Clients < 2 or Max_Clients > 50 then
			raise Usage_Error;
		end if;
	end Arguments_Control;
		
	Port: Integer;
	Max_Clients: Integer;
	Server_EP: LLU.End_Point_Type;
	Command: Character;
begin
	Arguments_Control(Port, Max_Clients);
	
	Server_EP := LLU.Build(LLU.To_IP(LLU.Get_Host_Name), Port);
	LLU.Bind(Server_EP, SH.Chat_Handler'Access);

	loop
		ATIO.Get_Immediate(Command);
		case Command is
			when 'l' | 'L' =>
				SH.Active_Collection;
			when 'o' | 'O' =>
				SH.Inactive_Collection;
			when others =>
				null;
		end case;
	end loop;
		
exception
	when Usage_Error =>
		ATIO.Put_Line("usage: chart_server <port> <max_clients [ 2 - 50 ]>");
		LLU.Finalize;
end Chat_Server_2;
