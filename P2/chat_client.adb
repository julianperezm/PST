with Ada.Text_IO;
with Ada.Strings.Unbounded;
with Ada.Command_Line;
with Chat_Messages;
with Lower_Layer_UDP;

procedure Chat_Client is
	package ATIO renames Ada.Text_IO;
	package ASU renames Ada.Strings.Unbounded;
	package ACL renames Ada.Command_Line;
	package CM renames Chat_Messages;
	package LLU renames Lower_Layer_UDP;
	use Type CM.Message_Type;
	use type ASU.Unbounded_String;

	Usage_Error: Exception;

	procedure Arguments_Control(Hostname: out ASU.Unbounded_String; Port: out Integer; Nickname: out ASU.Unbounded_String) is
		
	begin 
		if ACL.Argument_Count /=3 then 
			raise Usage_Error;
		else
			Hostname:= ASU.To_Unbounded_String(ACL.Argument(1));
			Port := Integer'value(ACL.Argument(2));
			Nickname:= ASU.To_Unbounded_String(ACL.Argument(3));
		end if;
	end Arguments_Control;

	procedure Init_Message(Buffer: Access LLU.Buffer_Type;Client_EP: in LLU.End_Point_Type; Nickname:in ASU.Unbounded_String) is
	begin
		LLU.Reset(Buffer.all);

		CM.Message_Type'Output(Buffer, CM.Init);
		LLU.End_Point_Type'Output(Buffer, Client_EP);
		ASU.Unbounded_String'Output(Buffer, Nickname);
	end Init_Message;

	procedure Write_Message (Buffer: Access LLU.Buffer_Type;Client_EP: in LLU.End_Point_Type; Comment: in ASU.Unbounded_String) is 
	begin 
		LLU.Reset(Buffer.all);

		CM.Message_Type'Output(Buffer, CM.Writer);
		LLU.End_Point_Type'Output(Buffer, Client_EP);
		ASU.Unbounded_String'Output(Buffer, Comment);
	end Write_Message;

	procedure Writer_Mode(Client_EP: in out LLU.End_Point_Type; Server_EP: LLU.End_Point_Type) is 
		Comment: ASU.Unbounded_String;
		Buffer: aliased LLU.Buffer_Type(1024);
	begin
		loop
			ATIO.Put("Message: ");
			Comment:= ASU.To_Unbounded_String(ATIO.Get_Line);
			if Comment /= ".quit" then 
				Write_Message(Buffer'Access,Client_EP, Comment);
				LLU.Send(Server_EP, Buffer'Access);
			end if;
			exit when Comment = ".quit";
		end loop;
	end Writer_Mode;


	procedure Reader_Mode(CLient_EP: in LLU.End_Point_Type; MT: out CM.Message_Type; Nickname: out ASU.Unbounded_String;
						  Comment: out ASU.Unbounded_String) is 
		Expired : Boolean;
		Buffer: aliased LLU.Buffer_Type(1024);
	begin
		loop	
			LLU.Reset(Buffer);
			LLU.Receive(Client_EP, Buffer'Access, 3.0, Expired);
			if not Expired then 
				MT:= CM.Message_Type'Input(Buffer'Access);
				if MT = CM.Server then 
					Nickname:= ASU.Unbounded_String'Input(Buffer'Access);
					Comment := ASU.Unbounded_String'Input(Buffer'Access);
					ATIO.Put_Line(ASU.To_String(Nickname) & ":" & ASU.To_String(Comment));
				end if;
			end if;
		end loop;
	end Reader_Mode;

	Hostname: ASU.Unbounded_String;
	Port:Integer;
	Nickname: ASU.Unbounded_String;
	Server_EP: LLU.End_Point_Type;
	Client_EP: LLU.End_Point_Type;
	Buffer: aliased LLU.Buffer_Type(1024);
	MT:CM.Message_Type;
	Comment: ASU.Unbounded_String;
begin
	Arguments_Control(Hostname, Port,Nickname);

	Server_EP:= LLU.Build(LLU.To_IP(ASU.To_String(Hostname)), Port);

	LLU.Bind_Any(Client_EP);

	Init_Message(Buffer'Access,Client_EP, Nickname);
	LLU.Send(Server_EP, Buffer'Access);

	if Nickname = "reader" then 
		Reader_Mode(Client_EP, MT, Nickname, Comment);
	else 
		Writer_Mode(Client_EP, Server_EP);
	end if;
	LLU.Finalize;

Exception
	when Usage_Error =>
		ATIO.Put_Line("usage: <Hostname> <Port> <Nickname>");
		
end Chat_Client;

