with Ada.Unchecked_Deallocation;

package body Maps_G is 
    procedure Free is new Ada.Unchecked_Deallocation(Cell, Cell_A);

	procedure Get (M : Map; Key : in Key_Type; Value : out Value_Type; Success : out Boolean) is 
		P_Aux:Cell_A:= M.P_First;
	begin 
		Success:= False;
		while not Success and P_Aux /= null loop
			if P_Aux.Key = Key then 
				Value:= P_Aux.Value;
				Success := True;
			end if;
			P_Aux:= P_Aux.Next;
		end loop;
	end Get;
	
	procedure Put (M : in out Map; Key : Key_Type; Value : Value_Type) is
	Found: Boolean:= False;
	P_Aux: Cell_A:= M.P_First;
	P_New: Cell_A;
	begin
		while not Found and P_Aux /= null loop
			if P_Aux.Key = Key then 
				P_Aux.Value := Value;
				Found:= True; 
			end if;
			P_Aux:= P_Aux.Next;
		end loop;
		
		if not Found then 
			if M.Length < Max_Length then
				P_New:= new Cell'(Key, Value, null);
				if M.Length = 0 then 
					M.P_First:= P_New;
				else 
					P_Aux:= M.P_First;
					while P_Aux.Next /= null loop 
						P_Aux:= P_Aux.Next;
					end loop;
					P_Aux.Next:= P_New;
				end if;
				M.Length:= M.Length + 1;	
			else 
				raise Full_Map;
			end if;
		end if;
	end Put;
								
	procedure Delete (M : in out Map; Key : in Key_Type; Success : out Boolean) is
		P_Current: Cell_A:= M.P_First;
		P_Previous: Cell_A:= Null;
	begin
		Success:= False;
		while not Success and P_Current /= null loop
			if P_Current.Key = Key then 
				Success:= True;
				M.Length := M.Length - 1;
				if P_Previous /= null then 
					P_Previous.Next := P_Current.Next;
				end if;
				
				if M.P_First = P_Current then 
					M.P_First := M.P_First.Next;
				end if;
				Free(P_Current);
			else 
				P_Previous := P_Current;
				P_Current := P_Current.Next;
			end if;
		end loop;
	end Delete;
	
	function Map_Length (M : Map) return Natural is 
	begin
		return M.Length;
	end Map_Length;
	
	function First (M: Map) return Cursor is 
	begin
		return(M => M, Element_A => M.P_First);
	end First;
	
	procedure Next (C: in out Cursor) is 
	begin
		if C.Element_A /= null then 
			C.Element_A := C.Element_A.Next;
		end if;
	end Next;
	
	function Has_Element (C: Cursor) return Boolean is 
	begin
		If C.Element_A /= null then 
			return True;
		else
			return False;
		end if;
	end Has_Element;
	
	function Element (C: Cursor) return Element_Type is 
	begin
		if C.Element_A /= null then 
			Return (Key => C.Element_A.Key, Value => C.Element_A.Value);
		else 
			raise No_Element;
		end if;
	end Element;
end Maps_G;
