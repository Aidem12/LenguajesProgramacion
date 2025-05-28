% Hechos: Definición de materias disponibles
materia(matematicas).
materia(algebra).
materia(programacion).
materia(ingles).
materia(matematicas_discretas).
materia(ensamblador).
materia(calculo).
materia(redes).
materia(estructura_datos).
materia(bases_datos).
materia(inteligencia_artificial).
materia(compiladores).
materia(sistemas_operativos).
materia(topicos_programacion).

% Horarios con cupo actual: materia(Nombre, HoraInicio, HoraFin, Dia, CupoActual)
materia(matematicas, 8, 10, lunes, 6).
materia(algebra, 9, 11, lunes, 5).
materia(programacion, 10, 12, martes, 9).
materia(ingles, 8, 10, martes, 10).
materia(matematicas_discretas, 8, 10, miercoles, 6).
materia(ensamblador, 10, 12, miercoles, 9).
materia(calculo, 9, 11, jueves, 4).
materia(redes, 11, 13, jueves, 10).  % lleno
materia(estructura_datos, 7, 9, viernes, 3).
materia(bases_datos, 9, 11, viernes, 9).
materia(inteligencia_artificial, 11, 13, viernes, 7).
materia(compiladores, 7, 9, lunes, 8).
materia(sistemas_operativos, 10, 12, jueves, 6).
materia(topicos_programacion, 8, 10, miercoles, 10).  % lleno

% Materia por semestre
materia_semestre(matematicas, 1).
materia_semestre(algebra, 1).
materia_semestre(programacion, 1).
materia_semestre(ingles, 2).
materia_semestre(matematicas_discretas, 2).
materia_semestre(ensamblador, 2).
materia_semestre(calculo, 3).
materia_semestre(redes, 3).
materia_semestre(estructura_datos, 3).
materia_semestre(bases_datos, 4).
materia_semestre(inteligencia_artificial, 4).
materia_semestre(compiladores, 4).
materia_semestre(sistemas_operativos, 5).
materia_semestre(topicos_programacion, 5).

% Estudiantes con semestre
estudiante(alicia, 3).
estudiante(juan, 5).
estudiante(maria, 5).

% Materias cursadas por cada estudiante hasta su semestre actual
cursada(alicia, matematicas).
cursada(alicia, algebra).
cursada(alicia, ingles).
cursada(alicia, matematicas_discretas).
cursada(alicia, ensamblador).

cursada(juan, algebra).
cursada(juan, programacion).

cursada(maria, matematicas).
cursada(maria, algebra).
cursada(maria, programacion).
cursada(maria, ingles).
cursada(maria, matematicas_discretas).
cursada(maria, ensamblador).
cursada(maria, calculo).
cursada(maria, redes).
cursada(maria, estructura_datos).

% Prerrequisitos: prerequisito(MateriaActual, MateriaRequerida)
prerequisito(matematicas_discretas, matematicas).
prerequisito(ensamblador, programacion).
prerequisito(calculo, matematicas).
prerequisito(estructura_datos, ensamblador).
prerequisito(bases_datos, estructura_datos).
prerequisito(compiladores, estructura_datos).
prerequisito(sistemas_operativos, compiladores).
prerequisito(topicos_programacion, inteligencia_artificial).

% Empalmes -- Reglas --
empalme(X, Y) :-
    materia(X, H1, H2, D, _),
    materia(Y, H3, H4, D, _),
    X \= Y,
    H1 < H4,
    H3 < H2.

materias_con_empalme([], []).
materias_con_empalme([M | R], [M | Empalmadas]) :-
    member(Otra, R),
    empalme(M, Otra),
    materias_con_empalme(R, Empalmadas).
materias_con_empalme([M | R], Empalmadas) :-
    \+ (member(Otra, R), empalme(M, Otra)),
    materias_con_empalme(R, Empalmadas).

% Sin cupo
materias_sin_cupo([], []).
materias_sin_cupo([M | R], [M | SinCupo]) :-
    materia(M, _, _, _, 10),
    materias_sin_cupo(R, SinCupo).
materias_sin_cupo([M | R], SinCupo) :-
    materia(M, _, _, _, Cupo),
    Cupo < 10,
    materias_sin_cupo(R, SinCupo).

% Materias que no corresponden al semestre del estudiante
materias_no_corresponden(_, [], []).
materias_no_corresponden(Semestre, [M | R], [M | NoCorresponde]) :-
    materia_semestre(M, SM),
    SM \= Semestre,
    materias_no_corresponden(Semestre, R, NoCorresponde).
materias_no_corresponden(Semestre, [M | R], NoCorresponde) :-
    materia_semestre(M, SM),
    SM =:= Semestre,
    materias_no_corresponden(Semestre, R, NoCorresponde).

% Validar prerrequisitos
cumple_prerrequisitos(Estudiante, Materia) :-
    prerequisito(Materia, MateriaRequerida),
    \+ cursada(Estudiante, MateriaRequerida),
    format('Error: Para inscribir ~w debe haber cursado ~w previamente.~n', [Materia, MateriaRequerida]),
    !, fail.
cumple_prerrequisitos(_, _).

% Nueva regla: verificar que la materia no sea de un semestre superior
puede_inscribir_semestre(Estudiante, Materia) :-
    estudiante(Estudiante, SemestreEstudiante),
    materia_semestre(Materia, SemestreMateria),
    SemestreMateria =< SemestreEstudiante.

% Consulta completa con listas de conflictos
disponibilidad_horario(Estudiante, Materias, ConEmpalme, SinCupo, NoCorresponde) :-
    estudiante(Estudiante, Semestre),
    materias_con_empalme(Materias, ConEmpalme),
    materias_sin_cupo(Materias, SinCupo),
    materias_no_corresponden(Semestre, Materias, NoCorresponde).

% Consulta booleana simple
disponibilidad_horario(Estudiante, Materias) :-
    estudiante(Estudiante, Semestre),
    materias_con_empalme(Materias, []),
    materias_sin_cupo(Materias, []),
    materias_no_corresponden(Semestre, Materias, []).

% ---Sistema de inscripción ---

:- dynamic(inscrito/2).
:- dynamic(estudiante_actual/1).

% Máximo de materias permitidas
max_materias_por_estudiante(5).

% Verifica si hay empalme con inscritas
hay_empalme_con_inscritas(Estudiante, Materia) :-
    inscrito(Estudiante, OtraMateria),
    empalme(Materia, OtraMateria).

% Verifica si puede inscribir más materias
puede_inscribir_mas(Estudiante) :-
    findall(M, inscrito(Estudiante, M), Lista),
    length(Lista, N),
    max_materias_por_estudiante(Max),
    N < Max.

% Inscripción modificada con validación de prerrequisitos y semestre
inscribir(Estudiante, Materia) :-
    estudiante(Estudiante, _),
    materia(Materia, _, _, _, Cupo),
    Cupo < 10,
    puede_inscribir_semestre(Estudiante, Materia),
    \+ cursada(Estudiante, Materia),
    not(inscrito(Estudiante, Materia)),
    \+ hay_empalme_con_inscritas(Estudiante, Materia),
    puede_inscribir_mas(Estudiante),
    cumple_prerrequisitos(Estudiante, Materia),
    assertz(inscrito(Estudiante, Materia)).

% Dar de baja materias inscritas
dar_de_baja(Estudiante, Materia) :-
    inscrito(Estudiante, Materia),
    retract(inscrito(Estudiante, Materia)),
    write('Materia dada de baja correctamente.'), nl.

dar_de_baja(Estudiante, Materia) :-
    \+ inscrito(Estudiante, Materia),
    write('No estás inscrito en esa materia.'), nl.

% Ver materias inscritas
ver_inscritas(Estudiante, Lista) :-
    findall(Materia, inscrito(Estudiante, Materia), Lista).

% Ver materias disponibles modificado
materias_disponibles(Estudiante, Disponibles) :-
    estudiante(Estudiante, _),
    findall(M, (
               materia(M, _, _, _, Cupo), 
               Cupo < 10, 
               puede_inscribir_semestre(Estudiante, M),
               \+ cursada(Estudiante, M),
               \+ hay_empalme_con_inscritas(Estudiante, M),
               \+ inscrito(Estudiante, M),
               cumple_prerrequisitos(Estudiante, M)
               ), Disponibles).

% Mostrar horario
mostrar_horario(Estudiante) :-
    findall((M, Dia, H1, H2), (
                              inscrito(Estudiante, M),
                              materia(M, H1, H2, Dia, _)
                              ), Materias),
    sort(Materias, Ordenadas),
    nl, write('Horario de '), write(Estudiante), write(':'), nl,
    forall(member((M, D, H1, H2), Ordenadas),(
                                             format('~w: ~w de ~d a ~d~n', [D, M, H1, H2])
                                             )
           ).

% Verificar si el estudiante existe
estudiante_existe(Nombre) :-
    estudiante(Nombre, _).

% Guardar estudiante actual
guardar_estudiante_actual(Nombre) :-
    retractall(estudiante_actual(_)),
    assertz(estudiante_actual(Nombre)).

% Obtener estudiante actual
obtener_estudiante_actual(Nombre) :-
    estudiante_actual(Nombre).

% Mostrar información del estudiante
mostrar_info_estudiante(Nombre) :-
    estudiante(Nombre, Semestre),
    nl,
    write('Estudiante: '), write(Nombre), nl,
    write('Semestre: '), write(Semestre), nl.

% Menú interactivo con selección de estudiante al inicio
iniciar_sistema :-
    write('=== SISTEMA DE INSCRIPCIÓN DE MATERIAS ==='), nl,
    write('Ingrese su nombre: '), 
    read(Nombre),
    (estudiante_existe(Nombre) ->
        guardar_estudiante_actual(Nombre),
        mostrar_info_estudiante(Nombre),
        menu;
        write('Estudiante no encontrado. Verifique el nombre o contacte al administrador.'), nl,
        iniciar_sistema
    ).

% Menú principal
menu :-
    nl,
    writeln('=== MENÚ PRINCIPAL ==='),
    obtener_estudiante_actual(Estudiante),
    format('Usuario actual: ~w~n', [Estudiante]),
    writeln('1. Ver materias disponibles'),
    writeln('2. Inscribir materia'),
    writeln('3. Ver materias inscritas'),
    writeln('4. Ver horario'),
    writeln('5. Analizar una lista de materias (ver conflictos)'),
    writeln('6. Dar de baja una materia'),
	writeln('7. Cambiar usuario'),
	writeln('8. Salir'),
    writeln('Seleccione una opción: '),
    read(Opcion),
    ejecutar_opcion(Opcion).

% Opciones del menú modificadas que utilizan estudiante actual
ejecutar_opcion(1) :-
    obtener_estudiante_actual(Est),
    materias_disponibles(Est, M),
    write('Materias disponibles: '), write(M), nl,
    menu.
ejecutar_opcion(2) :-
    obtener_estudiante_actual(Est),
    write('Ingrese materia a inscribir: '), read(Mat),
    (inscribir(Est, Mat) ->  
        write('Materia inscrita correctamente.'), nl;
        write('No se pudo inscribir. Verifique empalmes, cupo, límite o semestre.'), nl
    ), menu.
ejecutar_opcion(3) :-
    obtener_estudiante_actual(Est),
    ver_inscritas(Est, M),
    write('Materias inscritas: '), write(M), nl,
    menu.
ejecutar_opcion(4) :-
    obtener_estudiante_actual(Est),
    mostrar_horario(Est),
    menu.
ejecutar_opcion(5) :-
    obtener_estudiante_actual(Nombre),
    write('Ingrese la lista de materias entre corchetes (ej. [mate, redes]): '),
    read(Lista),
    disponibilidad_horario(Nombre, Lista, Empalmes, SinCupo, NoCorresponden),
    writeln('--- Conflictos detectados ---'),
    format('Empalmes: ~w~n', [Empalmes]),
    format('Sin cupo: ~w~n', [SinCupo]),
    format('Fuera de semestre: ~w~n', [NoCorresponden]),
    menu.
ejecutar_opcion(6) :-
    obtener_estudiante_actual(Est),
    write('Ingrese materia a dar de baja: '), read(Mat),
    dar_de_baja(Est, Mat),
    menu.
ejecutar_opcion(7) :-
    writeln('Cambio de usuario...'),
    iniciar_sistema.
ejecutar_opcion(8) :-
    write('Saliendo del sistema...'), nl.
ejecutar_opcion(_) :-
    write('Opción inválida.'), nl,
    menu.

% Punto de entrada al sistema
:- dynamic(sistema_iniciado/0).

% Iniciar el sistema solo una vez
:- 
    \+ sistema_iniciado,
    assertz(sistema_iniciado),
    iniciar_sistema.