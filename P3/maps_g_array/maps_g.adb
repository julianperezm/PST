with Ada.Unchecked_Deallocation;

package body Maps_G is 
    
	procedure Get (M : Map; Key : in Key_Type; Value : out Value_Type; Success : out Boolean) is 
		Index: Positive:= 1;
	begin
		Success:= False;
		while not Success and Index <= Max_length loop
			if M.P_Array(Index).Full and M.P_Array(Index).Key = key then
					Value:= M.P_Array(Index).Value;
					Success:= True;
				end if ;
			Index:= Index + 1;
		end loop;
	end Get;
	
	procedure Put (M : in out Map; Key : Key_Type; Value : Value_Type) is
	Found: Boolean:= False;
	Index: Positive:= 1;
	begin
		while not Found and Index <= Max_Length loop
		Found:= M.P_Array(Index).Full and M.P_Array(Index).Key = key;
		if Found then
			M.P_Array(Index).Value := Value;
		end if;
		Index:= Index + 1;
		end loop;
		
		if not Found then 
			if M.Length < Max_Length then 
				Index:=1;
				while not Found and Index <= Max_Length loop
					Found := M.P_Array(Index).Full = False;
					if Found then 
						M.P_Array(Index).Key := Key;
						M.P_Array(Index).Value := Value;
						M.P_Array(Index).Full := True;
						
						M.Length := M.Length + 1;
					end if;
					Index := Index + 1;
				end loop;
			else
				raise Full_Map;
			end if;
		end if;
	end Put;
	
	procedure Delete (M : in out Map; Key : in Key_Type; Success : out Boolean) is 
		Index: Positive := 1;
	begin
		Success:= False;
		while not Success and Index <= Max_Length loop
			if M.P_Array(Index).Full and  M.P_Array(Index).Key = Key then 
					M.P_Array(Index).Full := False;
					Success := True;
					M.Length := M.Length - 1;
				end if;
			
			Index:= Index + 1;
		end loop;
	end Delete;
	
	function Map_Length (M : Map) return Natural is 
	begin
		return M.Length;
	end Map_Length;
	
	function First (M: Map) return Cursor is
		Found : Boolean:= False;
		Index : positive:= 1;
	begin
		if M.Length /= 0 then 
			while not Found and Index <= Max_Length loop
				Found := M.P_Array(Index).Full; 
				if not Found then 
					Index := Index + 1;
				end if;
			end loop;
			return (M => M, Element_I => Index);
		else 
			return (M => M, Element_I => 0);
		end if;
	end First;
	
	procedure Next (C: in out Cursor) is 
		Found: Boolean := False;
	begin
		if C.Element_I /= 0 then 
			C.Element_I:= C.Element_I +1; 
			while not Found and C.Element_I <= Max_Length loop
				Found:= C.M.P_Array(C.Element_I).full; 
				if not Found then 
					C.Element_I := C.Element_I + 1;
				end if;
			end loop;
			
			if not Found then 
				C.Element_I := 0;
			end if;
		end if;
	end Next;
	
	function Has_Element (C: Cursor) return Boolean is 
	begin
		if C.Element_I /= 0 then 
			return True;
		else 
			return False;
		end if;
	end Has_Element;
	
	function Element (C: Cursor) return Element_Type is 
	begin
		if C.Element_I /= 0 then 
			return (Key => C.M.P_Array(C.Element_I).Key, Value => C.M.P_Array(C.Element_I).Value);
		else 
			raise No_Element;
		end if;
	end Element;
end Maps_G;
