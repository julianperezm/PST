with Ada.Strings.Unbounded;
with Client_Handler;
with Ada.Command_Line;
with Lower_Layer_UDP;
with Chat_Messages;
with Ada.Text_IO;

procedure Chat_Client_2 is
	package ASU renames Ada.Strings.Unbounded;
	package CH renames Client_Handler;
	package ACL renames Ada.Command_Line;
	package LLU renames Lower_Layer_UDP;
	package CM renames Chat_Messages;
	package ATIO renames Ada.Text_IO;
	
	use type ASU.Unbounded_String;
	use type CM.Message_Type;

	Usage_Error: Exception;

	procedure Arguments_Control(Hostname: out ASU.Unbounded_String; Port: out Integer; 
								Nickname: out ASU.Unbounded_String) is 
	begin
		if ACL.Argument_Count /= 3 then
		raise Usage_Error;
		end if;
		
		Hostname:= ASU.To_Unbounded_String(ACL.Argument(1));
		Port:= Integer'Value(ACL.Argument(2));
		Nickname:= ASU.To_Unbounded_String(ACL.Argument(3));
	end Arguments_Control;
	
	procedure Init_Message(P_Buffer: Access LLU.Buffer_type; Client_EP_Receive: in LLU.End_Point_type;
						   Client_EP_Handler: in LLU.End_Point_Type; Nick: in ASU.Unbounded_String) is 
	begin
		LLU.Reset(P_Buffer.all);
		CM.Message_Type'Output(P_Buffer, CM.Init);
		LLU.End_Point_Type'Output(P_Buffer, Client_EP_Receive);
		LLU.End_Point_Type'Output(P_Buffer, CLient_EP_Handler);
		ASU.Unbounded_String'Output(P_Buffer, Nick);
	end Init_Message;
	
	procedure Writer_Message(P_Buffer: Access LLU.Buffer_type; Client_EP_Handler: in LLU.End_Point_type;
						   Nick: in ASU.Unbounded_String; Comment: in ASU.Unbounded_String) is 
	begin
		LLU.Reset(P_Buffer.all);
		CM.Message_Type'Output(P_Buffer, CM.Writer);
		LLU.End_Point_Type'Output(P_Buffer, Client_EP_Handler);
		ASU.Unbounded_String'Output(P_Buffer, Nick);
		ASU.Unbounded_String'Output(P_Buffer, Comment);
	end Writer_Message;
	
	procedure Logout_Message(P_Buffer: Access LLU.Buffer_Type; Client_EP_Handler: in LLU.End_Point_Type;
							 Nick : in ASU.Unbounded_String) is 
	begin
		LLU.Reset(P_Buffer.all);
		CM.Message_type'Output(P_Buffer, CM.Logout);
		LLU.End_Point_type'Output(P_Buffer, Client_EP_Handler);
		ASU.Unbounded_String'Output(P_Buffer, Nick);
	end Logout_Message;
	
	procedure Acogido_Mode(Comment: out ASU.Unbounded_String; Client_EP_Handler: in out LLU.End_Point_type;
						   Nickname: in out ASU.Unbounded_String; P_Buffer: Access LLU.Buffer_Type; Server_EP: in out LLU.End_Point_Type) is 
	
	begin
		ATIO.Put_Line(("Welcome " )& ASU.To_string(Nickname));
		loop
			ATIO.Put(">>");
			Comment:= ASU.To_Unbounded_String(ATIO.Get_Line);
			if Comment /= ".quit" then 
				Writer_Message(P_Buffer, Client_EP_Handler, Nickname, Comment);
				LLU.Send(Server_EP, P_Buffer);
			end if;
		exit when Comment =".quit";
		end loop;
		Logout_Message(P_Buffer, Client_EP_Handler, Nickname);
		LLU.Send(Server_EP, P_Buffer);
	end Acogido_Mode;
	
	Client_EP_Receive: LLU.End_Point_Type;
	Client_EP_Handler: LLU.End_Point_Type;
	Hostname: ASU.Unbounded_String;
	Port: Integer;
	Nickname: ASU.Unbounded_String;
	Server_EP: LLU.End_Point_Type;
	P_Buffer: aliased LLU.Buffer_Type(1024);
	Expired: Boolean;
	MT: CM.Message_Type;
	Acogido: Boolean;
	Comment: ASU.Unbounded_String;

begin
	Arguments_Control(Hostname, Port, Nickname);
	
	Server_EP := LLU.Build(LLU.To_IP(ASU.To_String(Hostname)), Port);
	
	LLU.Bind_Any(CLient_EP_Receive);
	LLU.Bind_Any(CLient_EP_Handler, CH.Chat_Handler'Access);
	
	Init_Message(P_Buffer'Access, Client_EP_Receive, CLient_EP_Handler, Nickname);
	LLU.Send(Server_EP, P_Buffer'Access);
	
	LLU.Reset(P_Buffer);
	LLU.Receive(Client_EP_Receive, P_Buffer'Access, 5.0, Expired);
	if not Expired then 
		ATIO.Put("Mini-Chat v2.0: ");
		MT:= CM.Message_Type'Input(P_Buffer'Access);
		if MT = CM.Welcome then 
			Acogido:= Boolean'Input(P_Buffer'Access);
			if Acogido then 
				Acogido_Mode(Comment, Client_EP_Handler, Nickname, P_Buffer'Access, Server_EP);
			else
				ATIO.Put_Line("IGNORED new user " & ASU.To_String(Nickname) & ", nick already used");
			end if;
		end if;
	else 
		ATIO.Put_Line("Server unreachable");
	end if;
	LLU.Finalize;

Exception 
	when Usage_Error =>
		ATIO.Put_Line("usage: chat_client_2 <Hostname> <Port> <Nickname /= Server>");
		LLU.Finalize;
			
end Chat_Client_2;
