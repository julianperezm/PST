with Ada.Text_IO;
with Ada.Command_Line;
with Ada.Strings.Unbounded;
with Lower_Layer_UDP;
with Chat_Messages;
with Client_Collections;

procedure Chat_Server is
	package ATIO renames Ada.Text_IO;
	package ASU renames Ada.Strings.Unbounded;
	package ACL renames Ada.Command_Line;
	package CM renames Chat_Messages;
	package LLU renames Lower_Layer_UDP;
	package CC renames Client_Collections;
	use Type CM.Message_Type;
	use type ASU.Unbounded_String;

	Usage_Error: Exception;


	procedure Arguments_Control (Port: out Integer; Server_Password: out ASU.Unbounded_String) is 

	begin
		if ACL.Argument_Count /= 2 then 
			raise Usage_Error;
		else
			Port:= Integer'Value(ACL.Argument(1));
			Server_Password:= ASU.To_Unbounded_String(ACL.Argument(2));
		end if;
	end Arguments_Control;

	procedure Server_Message(Buffer: Access LLU.Buffer_Type; Nickname: in ASU.Unbounded_String; Text: in ASU.Unbounded_String ) is 
	begin 
		LLU.Reset(Buffer.all);

		CM.Message_Type'Output(Buffer, CM.Server);
		ASU.Unbounded_String'Output(Buffer, Nickname);
		ASU.Unbounded_String'Output(Buffer, Text);
	end Server_Message;

	procedure Collection_Data_Message(Buffer: access LLU.Buffer_Type; Data: in ASU.Unbounded_String) is 
	begin
		LLU.Reset(Buffer.all);

		CM.Message_Type'Output(Buffer, CM.Collection_Data);
		ASU.Unbounded_String'Output(Buffer, Data);
	end Collection_Data_Message;

	Port:Integer;
	Server_EP: LLU.End_Point_Type;
	Client_EP: LLU.End_Point_Type;
	Buffer: Aliased LLU.Buffer_Type(1024);
	Expired: Boolean;
	Nickname: ASU.Unbounded_String;
	Text:ASU.Unbounded_String;
	Readers_Collection: CC.Collection_Type;
	Writers_Collection: CC.Collection_Type;
	MT: CM.Message_Type;
	Comment : ASU.Unbounded_String;
	Admin_EP : LLU.End_Point_Type;
	Password: ASU.Unbounded_String;
	Server_Password: ASU.Unbounded_String;
	Data: ASU.Unbounded_String;
begin
	Arguments_Control(Port, Server_Password);
	Server_EP := LLU.Build(LLU.To_IP(LLU.Get_Host_Name), Port);
	LLU.Bind(Server_EP);
	loop
		LLU.Reset(Buffer);
		LLU.Receive(Server_EP, Buffer'Access, 2.0, Expired);
		if not Expired then 
			MT:= CM.Message_Type'Input(Buffer'Access);
			case MT is 
				When CM.Init =>
					ATIO.Put("INIT received from ");
					Client_EP:= LLU.End_Point_Type'Input(Buffer'Access);
					Nickname:= ASU.Unbounded_String'Input(Buffer'Access);
					if Nickname = "reader" then 
						CC.Add_Client(Readers_Collection, Client_EP, Nickname, False);
						ATIO.Put_Line(ASU.To_String(Nickname));
					else
						begin
							CC.Add_Client(Writers_Collection, Client_EP, Nickname, True);
							ATIO.Put_Line(ASU.To_String(Nickname));
							Server_Message(Buffer'Access,ASU.To_Unbounded_String("Server"), ASU.To_Unbounded_String( ASU.To_String(Nickname) & " joins the  chat"));
							CC.Send_To_all(Readers_Collection, Buffer'Access);
						Exception
							when CC.Client_Collection_Error =>
								ATIO.Put_Line(ASU.To_String(Nickname) & " IGNORED, nick already used");
						end;
					end if;
				when CM.Writer =>
					ATIO.Put("WRITER received from ");
					Client_EP:= LLU.End_Point_Type'Input(Buffer'Access);
					Comment:= ASU.Unbounded_String'Input(Buffer'Access);
					begin
						Nickname:= CC.Search_Client(Writers_Collection, Client_EP);
						ATIO.Put_Line(ASU.To_String(Nickname) & ": " & ASU.To_String(Comment));

						Server_Message(Buffer'Access,Nickname, Comment);
						CC.Send_To_all(Readers_Collection, Buffer'Access);
					Exception
						when CC.Client_Collection_Error =>
							ATIO.Put_Line("unknown client. IGNORED");
					end;
				when CM.Collection_Request =>
					Admin_EP:=LLU.End_Point_Type'Input(Buffer'Access);
					Password:= ASU.Unbounded_String'Input(Buffer'Access);
					if Server_Password = Password then 
						ATIO.Put_Line("LIST REQUEST received"); 
						Data:= ASU.To_Unbounded_String(CC.Collection_Image(Writers_Collection));
						Collection_Data_Message(Buffer'Access, Data);
						LLU.Send(Admin_EP, Buffer'Access);
					else
						ATIO.Put_Line("LIST_REQUEST received. IGNORED, incorrect password");
					end if;
				when  CM.Ban => 
					begin
					Password:= ASU.Unbounded_String'Input(Buffer'Access);
					Nickname:= ASU.Unbounded_String'Input(Buffer'Access);
						if Server_Password = Password then 
							CC.Delete_Client(Writers_Collection, Nickname);
							ATIO.Put_Line("BAN received for " & ASU.To_String(Nickname));
						end if;
					Exception
						when CC.Client_Collection_Error =>
							ATIO.Put_Line("BAN received for " & ASU.To_String(Nickname) & ". IGNORED, nick not found");
					end;
				when CM.Shutdown => 
					Password:= ASU.Unbounded_String'Input(Buffer'Access);
					if Server_Password = Password then
						ATIO.Put_Line("SHUTDOWN received");
						Expired:= True;
					else
						ATIO.Put_Line("SHUTDOWN received.IGNORED, incorrect password");
					end if;
				when others =>
					null;
			end case;
		end if;
		exit when MT = CM.Shutdown;
	end loop;
	LLU.Finalize;
Exception 
	When Usage_Error=>
		ATIO.Put_Line("Usage: <Port>");
	LLU.Finalize;
end Chat_Server;
