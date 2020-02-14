with Ada.Text_IO;
with Ada.Unchecked_Deallocation;



Package body Client_Collections is
	Package ATIO renames Ada.Text_IO;

	use type LLU.End_Point_Type;
	use type ASU.Unbounded_String;

	procedure Add_Client (Collection: in out Collection_Type; EP: in LLU.End_Point_Type; Nick: in ASU.Unbounded_String; Unique: in Boolean) is
		P_Aux: Cell_A:= Collection.P_First;
		P_New: Cell_A;
		Found: Boolean:= False;
	begin
		While not found and P_Aux/= null loop
			Found:= Nick = P_Aux.Nick;
			if not Found then 
				P_Aux:= P_Aux.Next;
			end if;
		end loop; 

		if (not Found) or (Found and not Unique) then 
			P_New:= new Cell'(EP, Nick, Collection.P_First);
			Collection.P_First:= P_New;
			Collection.Total := Collection.Total + 1;
		else
			raise Client_Collection_Error;
		end if; 
	end Add_Client;

	procedure Free is new Ada.Unchecked_Deallocation(Cell, Cell_A);

	procedure Delete_Client (Collection: in out Collection_Type; Nick: in ASU.Unbounded_String) is
		P_Aux: Cell_A;
		P_Next: Cell_A:= Collection.P_First;
		Found: Boolean:= False;
	begin	
		while not Found and P_Next /= null loop
			Found:= Nick = P_Next.Nick;
			if not Found then
				P_Aux:= P_Next;
				P_Next:= P_Next.Next;
			end if;
		end loop;

		if Found then 
			if P_Aux = null then 
				Collection.P_First:= Collection.P_First.Next;
			else 
				P_Aux.Next:= P_Next.Next;
			end if;
			Free(P_Next);
			Collection.Total:= Collection.Total - 1;
		else
			raise Client_Collection_Error;
		end if;
	end Delete_Client;

	function Search_Client (Collection: in Collection_Type; EP: in LLU.End_Point_Type) return ASU.Unbounded_String is
		P_Aux: Cell_A:= Collection.P_First;
		Found: Boolean:= False;
	begin

		while not Found and P_Aux /= null loop
			Found:= EP = P_Aux.Client_EP;
			if not Found then 
				P_Aux := P_Aux.Next;
			end if;
		end loop;

		if Found then 
			return P_Aux.Nick;
		else
			raise Client_Collection_Error;
		end if;
	end Search_Client;

	procedure Send_To_All (Collection: in Collection_Type; P_Buffer: access LLU.Buffer_Type) is 
		P_Aux: Cell_A:= Collection.P_First;
	begin
		while P_Aux /= null loop
			LLU.Send(P_Aux.Client_EP, P_Buffer);
			P_Aux:= P_Aux.Next;
		end loop;
	end Send_To_All;

	Function Split_EP(EP: in LLU.End_Point_Type) return ASU.Unbounded_String is 
		EP_Image: ASU.Unbounded_String;
		Port: Integer;
		IP: ASU.Unbounded_String;
		Position: Integer;
	begin
		EP_Image:= ASU.To_Unbounded_String(LLU.Image(EP));
		Position := ASU.Index(EP_Image,":");
		ASU.Tail(EP_Image, ASU.Length(EP_Image) - (Position + 1));

		Position:= ASU.Index(EP_Image, ",");
		IP:= ASU.Head(EP_Image, Position - 1);
		ASU.Tail(EP_Image, ASU.Length(EP_Image) - (Position + 1));

		Position:= ASU.Index(EP_Image, ":");
		Port:= Integer'Value(ASU.To_String(ASU.Tail(EP_Image, ASU.Length(EP_Image) - (Position + 1))));

		return IP & ASU.To_Unbounded_String(":") & Integer'Image(Port);
	end Split_EP;

	function Collection_Image (Collection: in Collection_Type) return String is 
		P_Aux: Cell_A := Collection.P_First;
		Sentence: ASU.Unbounded_String;
		Final_Sentence: ASU.Unbounded_String;
	begin
		While P_Aux /= null loop
			Sentence:= Split_EP(P_Aux.Client_EP) & " " & P_Aux.Nick;
			Final_Sentence:= Sentence & ASCII.LF & Final_Sentence;
			P_Aux:= P_Aux.Next;
		end loop;
		return ASU.To_String(Final_Sentence);
	end Collection_Image;


	
end Client_Collections;
