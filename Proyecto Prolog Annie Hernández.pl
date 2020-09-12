:-dynamic block/6,
          cell/3,
          row_restrictions/6, 
          column_restrictions/4,
          block_column_restrictions/5,
          block_row_restrictions/5.

%Establece las restricciones de las filas y colummnas del tablero, recibe 4 listas de listas 
%con las restricciones de los colores y los tamaños de cada bloque en cada fila y columna
%respectivamente. 
board(Row_Colors, Row_Sizes, Column_Colors, Column_Sizes):-
				length(Column_Colors, Row_Size),
				length(Row_Colors, Column_Size),
				columns(Column_Colors, Column_Sizes, 0, Column_Size),
				rows(Row_Colors, Row_Sizes, 0, Row_Size), !.

%Separa las restricciones para cada columna individual
columns([], [], _, _).
columns([Colors|Column_Colors], [Sizes|Column_Sizes], Index, Size):- 
				board_columns(Index, Colors, Sizes, Size),
				New_Index is Index + 1,
				columns(Column_Colors, Column_Sizes, New_Index, Size).

%Separa las restricciones para cada bloque de una columnna
board_columns(Column, Colors, Sizes, Column_Size):-
				assert(column_restrictions(Column, Colors, Sizes, Column_Size)),
				add_block_column_restrictions(Column, Colors, Sizes, 0, Column_Size).

%Separa las restricciones para cada fila individual
rows([], [], _, _).
rows([Colors|Row_Colors], [Sizes|Row_Sizes], Index, Size):- 
				board_rows(Index, Colors, Sizes, Size),
				New_Index is Index + 1,
				rows(Row_Colors, Row_Sizes, New_Index, Size).

%Agrega a las restricciones para cada fila la suma de los tamaños de los bloques y la cantidad de bloques del
%mismo tamaño que hay
board_rows(Row, Colors, Sizes, Row_Size):-
				sum_sizes(Sizes, 0, Sum),
				blocks_same_color(Colors, 0, Blocks_Same_Color),
				assert(row_restrictions(Row, Colors, Sizes, Blocks_Same_Color, Sum, Row_Size)),
				add_blocks_restrictions(Row, Colors, Sizes, 0),
				add_cells(Row, 0, Row_Size).

%Separa las restricciones para cada bloque de una fila y agrega la estructura de bloque que es con la que se 
%trabaja, esta contiene la fila, su índice en esta, su color, su tamaño y todas las columnnas posibles donde puede
%comenzar
add_blocks_restrictions(_, [], [], _).
add_blocks_restrictions(Row, [Color|Colors], [Size|Sizes], Index):-
				find_options(Row, Index, [Option|Options], Color),
				check_options([Option|Options], Color, Size, New_Options),
				assert(block_row_restrictions(Row, Index, Color, Size, New_Options)),
				assert(block(Row, Index, Option, Color, Size, New_Options)),
				Next_Index is Index + 1,
				add_blocks_restrictions(Row, Colors, Sizes, Next_Index).

%Inicializa la estructura de celda que representan el tablero, tiene fila, columna y color, inicialmente todas
%están en blanco.
add_cells(_, Column, Column).
add_cells(Row, Column, Row_Size):-
				assert(cell(Row, Column, white)),
				Next_Column is Column + 1,
				add_cells(Row, Next_Column, Row_Size).

%Separa las restricciones para cada bloque de una columna y agrega la estructura de bloque que es con la que se comprueba
%esta sabe la columna, su índice en esta, su color, su tamaño y la cantidad de filas.
add_block_column_restrictions(_, [], [], _, _).
add_block_column_restrictions(Column, [Color|Colors], [Size|Sizes], Index, Column_Size):-
				assert(block_column_restrictions(Column, Index, Color, Size, Column_Size)),
				Next_Index is Index + 1,
				add_block_column_restrictions(Column, Colors, Sizes, Next_Index, Column_Size).

%Determina la cantidad de bloques consecutivos del mismo color que hay en una fila
blocks_same_color(_, Current, Current).
blocks_same_color([Color|Colors], Current, Blocks_Same_Color):-
				member(New_Color, Colors),
				same_color(Color, New_Color, Same_Color),
				Count is Current + Same_Color,
				blocks_same_color(Colors, Count, Blocks_Same_Color).

%Calcula la suma de los tamaños de todos los bloques en una fila
sum_sizes([], Count, Count).
sum_sizes([Size|Sizes], Count, Sum):-
				New_Count is Size + Count,
				sum_sizes(Sizes, New_Count, Sum).

same_color(Color, Color, 1).
same_color(_, _, 0).

%Encuentra todas las columnas posibles donde puede comenzar un bloque en una fila, si  es el 1ro desde la 1ra columna
%hasta el tamaño de la fila menos la suma del tamaño de todos los bloques de esa fila menos la cantidad de bloques del
%mismo color que hay. Sino es el primero es desde la columna donde termina el bloque anterior, o esa columna más uno si
%ambos bloques son del mismo color, hasta la misma cantidad del caso anterior
find_options(Row, 0, Options, _):-
				row_restrictions(Row, _, _, Block_Same_Color, Block_Sizes, Size),
				Num_Options is Size - Block_Sizes - Block_Same_Color + 1,
				fill_list(0, Num_Options, Options).
find_options(Row, Index, Options, Block_Color):-
				row_restrictions(Row, _, _, Block_Same_Color, Block_Sizes, Size),
				Num_Options is Size - Block_Sizes - Block_Same_Color + 1,
				Prev_Index is Index - 1,
				block_row_restrictions(Row, Prev_Index, Color, Prev_Block_Size, [Column|_]),
				same_color(Block_Color, Color, Same_Color),
				First_Column is Column + Prev_Block_Size + Same_Color,
				fill_list(First_Column, Num_Options, Options).

fill_list(_, 0, []):-! .
fill_list(Index, Count, [Index|Options]):-
				New_Count is Count - 1,
				New_Index is Index + 1,
				fill_list(New_Index, New_Count, Options).

%Chequea que las opciones establecidas por fila sean válidas por los colores de las columnas (que en el conjunto de
%colores de las columnas de donde comienza el bloque hasta donde termina esté el color del bloque)
check_options([], _, _, []).
check_options([Option|Options], Block_Color, Block_Size, [Option | New_Options]):-
				check_block(Option, Block_Color, Block_Size), !, 
				check_options(Options, Block_Color, Block_Size, New_Options).
check_options([_|Options], Block_Color, Block_Size, New_Options):-
				check_options(Options, Block_Color, Block_Size, New_Options).

my_member(_, [], 0).
my_member(Block_Color, [Block_Color|_], 1).
my_member(Block_Color, [_|Colors], Member):- my_member(Block_Color, Colors, Member).

check_block(_, _, 0).
check_block(Option, Block_Color, Index):-
				New_Index is Index -1,
				New_Column is Option + New_Index,
				column_restrictions(New_Column, Colors, _, _), !,
				my_member(Block_Color, Colors, 1),
				check_block(Option, Block_Color, New_Index).






%Coloca cada bloque en una de las columnas posibles y luego comprueba si la distribución es válida
solve :- block_column_restrictions(_, _, _, _, Column_Size), !, fill_row(Column_Size), check_columns.



fill_row(0) :- !.
fill_row(I) :- J is I - 1, fill_row(J, 0), fill_row(J).

%Va llenando cada fila, colocando cada uno de sus bloques en una de las columnas posibles
fill_row(Row, Index) :- 
				block_row_restrictions(Row, Index, Color, Num, Options), !,
				member(Column, Options),
				block(Row, Index, Old_Column, _, _, _),
				Prev_Column is Column - 1, 
				not(cell(Row, Prev_Column, Color)),%en la columna anterior no puede haber un bloque del mismo color
				ExColumn is Column + Num - 1, 
				Next_Column is ExColumn + 1,
				not(cell(Row, Next_Column, Color)),%en la columna posterior no puede haber un bloque del mismo color
				findall(I, (between(Column, ExColumn, I), not(cell(Row, I, white))), []),%solo se puede escribir en  
				put_block(Row, Index, Column, Old_Column, Color, Num, Options),			 %celdas en blanco, para que
				New_Index is Index + 1,													 %no sobrescriba otros bloques
				fill_row(Row, New_Index).
fill_row(_, _).
				

%Hace el backtrack cuando el chequeo de las columnas falla, restableciendo las celdas en blanco y las 
%posiciones de los bloques.
put_block(Row, Index,  Column, Old_Column, Color, Num, Options):-
				retract(block(Row, Index, Old_Column, _, _, _)),
				assert(block(Row, Index, Column, Color, Num, Options)),
				paint_cells(Row, Column, Color, Num).
put_block(Row, Index,  Column, Old_Column, Color, Num, Options):-
				retract(block(Row, Index, Column, _, _, _)),
				assert(block(Row, Index, Old_Column, Color, Num, Options)),
				paint_cells(Row, Column, white, Num), !,
				fail.

%Pinta una celda específica en un color determinado
paint_cells(_, _, _, 0).
paint_cells(Row, Column, Color, Num):-
				retract(cell(Row, Column, _)),
				assert(cell(Row, Column, Color)),
				Next_Column is Column + 1,
				Count is Num - 1,
				paint_cells(Row, Next_Column, Color, Count), !.

%Revisa que se cumplan las restricciones por columna
check_columns :- check_columns(0, 0, 0, a), !.
check_columns(Row, Column, Index, Prev_Color):-
				block_column_restrictions(Column, Index, Color, Num, _),
				same_color(Color, Prev_Color, Same_Color), !,
				check_white_space(Row, Column, Same_Color, New_Row), !,
				check_cells(New_Row, Column, Color, Num, Next_Row),!,
				Next_Index is Index + 1,
				check_columns(Next_Row, Column, Next_Index, Color).
check_columns(_, Column, _, _):-
				Next_Column is Column + 1,
				row_restrictions(0, _, _, _, _, Size),
				Next_Column < Size, !,
				check_columns(0, Next_Column, 0, a).
check_columns(_, _, _, _).

%Revisa que la celda sea blanca o del color que se quiere, retorna la fila hasta donde chequeó
check_cells(Row, Column, Color, Num, Next_Row):-
				cell(Row, Column, white),!,
				New_Row is Row + 1,
				check_cells(New_Row, Column, Color, Num, Next_Row).
check_cells(Row, Column, Color, Num, Next_Row):-
				cell(Row, Column, Color),!,
				New_Row is Row + 1,
				Count is Num - 1, !,
				finish_block(New_Row, Column, Color, Count, Next_Row).

%Revisa que halla al menos un espacio en blanco entre dos bloque del mismo color
check_white_space(Row, _, 0, Row).
check_white_space(Row, Column, 1, Next_Row):-
				Next_Row is Row + 1,
				cell(Row, Column, white).

%Una vez encontrada la 1ra celda del color revisa que esté el resto del bloque
finish_block(Row, _, _, 0, Row).
finish_block(Row, Column, Color, Count, Next_Row):-
				cell(Row, Column, Color), !,
				New_Row is Row + 1,
				New_Count is Count - 1,
				finish_block(New_Row, Column, Color, New_Count, Next_Row).


print_board :- print_board(0, 0), !.
print_board(Row, Column) :- 
				cell(Row, Column, Color), 
				print_color(Color),
				tab(1), 
				Next_Column is Column + 1,
				row_restrictions(0, _, _, _, _, Size),
				Next_Column < Size,
				print_board(Row, Next_Column).
print_board(Row, _) :- 
				nl, column_restrictions(_, _, _, Size),
				Next_Row is Row + 1,
				Next_Row < Size,
				print_board(Next_Row, 0).
print_board(_, _).

print_color(white):- !, tab(1).
print_color(Color):- write(Color).

clean:- retractall(block(_,_,_,_,_,_)), 
		retractall(cell(_,_,_)),
		retractall(row_restrictions(_,_,_,_,_,_)),
		retractall(column_restrictions(_,_,_,_)),
		retractall(block_column_restrictions(_,_,_,_,_)),
		retractall(block_row_restrictions(_,_,_,_,_)).

%EJEMPLOS
all:- tutorial4, nl, tutorial5, nl, tutorial6, nl, tree, nl, house, nl, apple, nl, traffic_lights, nl, bee, nl, flower.

tutorial4 :- board([[b], [b], [g]], 
			   [[3], [5], [5]],
			   [[b, g], [b, g], [b, g], [b, g], [b,g]],
			   [[1,1],  [2,1],  [2,1],  [2,1], [1,1]]),
			   solve, print_board, clean.

tutorial5:- board([[b], [b], [g], [g, g]], 
			  [[3], [5], [5], [2, 2]],
			  [[b, g], [b, g], [b, g], [b, g], [b, g]],
			  [[1, 2], [2, 2], [2, 1], [2,2], [1,2]]),
			  solve, print_board, clean.

tutorial6:- board([[g], [g], [g], [g, b, g]], 
			  [[3], [5], [5], [2, 1, 2]],
			  [[g], [g], [g, b], [g], [g]],
			  [[3], [4], [3, 1], [4], [3]]),
			  solve, print_board, clean.

tree:-board([[g], [g], [g], [g, b, g], [b]], 
	  		[[3], [5], [5], [2, 1, 2], [1]],
	  		[[g], [g],  [g, b], [g], [g]], 
	  		[[3], [4], [3,2], [4], [3]]), 
	  		solve, print_board, clean.

house:-board([[b], [b], [b], [b, y, b], [b]], 
			[[1], [3], [5], [2, 1, 2], [5]],
			[[b], [b],  [b, y, b], [b], [b]], 
			[[3], [4], [3, 1, 1], [4], [3]]),
			solve, print_board, clean.

apple:-board([[b], [r, b, r], [r], [r], [r], [r], [r]], 
			[[2], [2, 1, 2], [7], [7], [7], [5], [3]],
	   		[[r], [r],  [r], [b, r], [b, r], [r], [r]], 
	   		[[3], [5], [6], [2, 5], [1, 6], [5], [3]]),
			solve, print_board, clean.

traffic_lights:-
board([[b], [b, r, b], [b, r, b], [b], [b, y, b],[b, y, b], [b], [b, g, b], [b, g, b], [b]], 
	  [[4], [1, 2, 1], [1, 2, 1], [4], [1, 2, 1], [1, 2, 1], [4], [1, 2, 1], [1, 2, 1], [4]], 
	  [[b], [b, r, b, y, b, g ,b], [b, r, b, y, b, g ,b], [b]], 
	  [[10], [1, 2, 1, 2, 1, 2, 1], [1, 2, 1, 2, 1, 2, 1], [10]]),
	  solve, print_board, clean.

bee:-board([[b, b], [b], [g, b, g], [g, y, g], [g, b, g],  [g, y, g], [g, b, g], [y]], 
		   [[1, 1], [2], [2, 2, 2], [2, 4, 2], [ 1, 4, 1], [2, 4, 2], [1, 4, 1], [2]], 
		   [[g, g], [g], [b, y, b, y, b], [b, y, b, y, b, y], [b, y, b, y, b, y], [b, y, b, y, b], [g], [g, g]],
		   [[2,2], [4], [1, 1, 1, 1, 1], [2, 1, 1, 1, 1, 1], [2, 1, 1, 1, 1, 1], [1, 1, 1, 1, 1], [4], [2,2]]),
		   solve, print_board, clean.

flower:-board([[r,r], [r], [r, y, r], [r], [r, g, r], [g], [g, g], [g, g], [g], [g]],
			  [[2,2], [5], [1, 1, 1], [5], [2, 1, 2], [1], [1, 1], [1, 3], [4], [2]],
			  [[r, r, g], [r, g], [r, y, r, g], [r, g], [r, r, g]],
			  [[2, 2, 2], [5, 2], [1, 1, 1, 6], [5, 2], [2, 2, 2]]),
			  solve, print_board.