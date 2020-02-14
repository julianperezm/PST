with Ada.Text_IO;
with ada.Unchecked_Deallocation;


package body Word_Lists is 
	package ATIO renames Ada.Text_IO;

	use type ASU.Unbounded_String;

	procedure Add_Word (List: in out Word_List_Type; Word: in ASU.Unbounded_String) is
    P_Aux: Word_List_Type:= List;
		P_New: Word_List_Type;
		Found: Boolean;
	begin
		Found:= False;
		while not Found and P_Aux /= null loop
			Found:= P_Aux.Word = Word;
			if Found then 
				P_Aux.Count:= P_Aux.Count + 1;
			end if;
			P_Aux:= P_Aux.Next;
		end loop;

		if not found then 
			P_New:= new Cell'(word, 1, null);
			if List = null then 
				List := P_New;
			else 	
				P_Aux:= List;
				while P_Aux.Next /= null loop
					P_Aux:= P_Aux.Next;
				end loop;
				P_Aux.Next := P_New;
			end if;
		end if;
	end Add_Word;
   	
   	procedure Free is new Ada.Unchecked_Deallocation(cell, Word_List_Type);

    procedure Delete_Word (List: in out Word_List_Type; Word: in ASU.Unbounded_String) is
    	P_Aux:Word_List_Type:= List;
    	P_Prev: Word_List_Type;
    	Found: Boolean;
    begin
    	Found := False;
    	while not found and P_Aux /= null loop 
    		Found:= P_Aux.Word = Word;
    		if not Found then 
    			P_Prev := P_Aux;
    			P_Aux := P_Aux.Next;
    		end if;
    	end loop;

    	if Found then 
    		if P_Prev = null then 
    			 list:= P_Aux.Next;
    		else 
    			P_Prev.Next := P_Aux.Next;
    		end if;
    		Free(P_Aux);
    	else
    		raise Word_List_Error;
    	end if;
    end Delete_Word;
   
    procedure Search_Word (List: in Word_List_Type; Word: in ASU.Unbounded_String; Count: out Natural) is 
    	P_Aux: Word_List_Type:= List;
    	Found : Boolean;
    begin
    	Found:= False;
    	Count:= 0;
    	while not Found and P_Aux /= null loop
    		Found := P_Aux.Word = Word;
    		if Found then 
    			Count := P_Aux.Count;
    		end if;
    		P_Aux:= P_Aux.Next;
    	end loop;
    end Search_Word;
   
   	procedure Max_Word (List: in Word_List_Type; Word: out ASU.Unbounded_String; Count: out Natural) is 
   		P_Aux: Word_List_Type:= List;
   	begin
   	  if List = null then 
        raise Word_List_Error;
    	end if;

   		Word:= List.Word;
   		Count:= List.Count;
    	while P_Aux /= null loop
    		if P_Aux.Count > Count then 
    			word := P_Aux.Word;
    			Count := P_Aux.Count;
    		end if;
   			P_Aux:= P_Aux.Next;
    	end loop;
   	end Max_Word;  
   
   	procedure Print_All (List: in Word_List_Type) is
   		P_Aux: Word_List_Type:= List; 	
   	begin
   		if List = null then
        	ATIO.Put_Line("No words.");
		else
        	while P_Aux /= null loop
				ATIO.Put_Line("|" & ASU.To_String(P_Aux.Word) & "| - " & Integer'Image(P_Aux.Count));
				 P_Aux := P_Aux.Next;
        	end loop;
		end if;
   	end Print_All;

    procedure Delete_List(List: in out Word_List_Type) is
      P_Aux : Word_List_Type:= List;
    begin   
        while List /= null loop
			P_Aux:= List;
        	List:= P_Aux.Next;
         	Free(P_Aux);
        end loop;

    end Delete_List;



end Word_Lists;