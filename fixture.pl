:- use_module(library(http/thread_httpd)).       % servidor HTTP
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/html_write)).         % generación de HTML
:- use_module(library(http/http_parameters)).    % lectura de parámetros POST/GET
:- use_module(library(http/http_client)).
:- use_module(library(http/http_files)).

:- use_module(library(http/http_server_files)).
:- multifile http:location/3.
http:location(static, '/static', []).
:- http_handler(static(.), serve_files_in_directory('static'), [prefix]).

:- use_module(library(apply)).    % include/3
:- use_module(library(lists)).    % sum_list/2, setof/3


serve_files_in_directory(Dir, Request) :-
		http_reply_from_files(Dir, [], Request).

:- dynamic partido/5.    % partido(ID, EquipoA, EquipoB, FechaHora, Estado)
:- dynamic resultado/2.   % resultado(ID, GolesA-GolesB)


% ——————————————————————————————————————————————————
% Definición completa de grupos A–L y partidos de fase de grupos
% IDs 1–72
% ——————————————————————————————————————————————————
:- dynamic grupo/2.
% Grupo A
grupo('México','A').        grupo('Sudáfrica','A').
grupo('Corea del Sur','A'). grupo('Europa D','A').
% Grupo B
grupo('Canadá','B').        grupo('Europa A','B').
grupo('Qatar','B').         grupo('Suiza','B').
% Grupo C
grupo('Brasil','C').        grupo('Marruecos','C').
grupo('Haití','C').         grupo('Escocia','C').
% Grupo D
grupo('Estados Unidos','D').grupo('Europa C','D').
grupo('Australia','D').     grupo('Paraguay','D').
% Grupo E
grupo('Alemania','E').      grupo('Curazao','E').
grupo('Costa de Marfil','E').grupo('Ecuador','E').
% Grupo F
grupo('Países Bajos','F').  grupo('Japón','F').
grupo('Europa B','F').      grupo('Túnez','F').
% Grupo G
grupo('Bélgica','G').       grupo('Egipto','G').
grupo('Irán','G').          grupo('Nueva Zelanda','G').
% Grupo H
grupo('España','H').        grupo('Cabo Verde','H').
grupo('Arabia Saudita','H').grupo('Uruguay','H').
% Grupo I
grupo('Francia','I').       grupo('Senegal','I').
grupo('Repechaje 2','I').   grupo('Noruega','I').
% Grupo J
grupo('Argentina','J').     grupo('Argelia','J').
grupo('Austria','J').       grupo('Jordania','J').
% Grupo K
grupo('Portugal','K').      grupo('Repechaje 1','K').
grupo('Uzbekistán','K').    grupo('Colombia','K').
% Grupo L
grupo('Inglaterra','L').    grupo('Croacia','L').
grupo('Ghana','L').         grupo('Panamá','L').

% ————————————
% Partidos G. A (IDs 1–6)
% ————————————
partido(1, 'México',         'Sudáfrica',          '2026-06-11 16:00', proximo).
partido(2, 'Corea del Sur',  'Europa D',           '2026-06-11 23:00', proximo).
partido(3, 'Europa D',       'Sudáfrica',          '2026-06-18 13:00', proximo).
partido(4, 'México',         'Corea del Sur',      '2026-06-18 22:00', proximo).
partido(5, 'Europa D',       'México',             '2026-06-24 22:00', proximo).
partido(6, 'Sudáfrica',      'Corea del Sur',      '2026-06-24 22:00', proximo).

% ————————————
% Partidos G. B (7–12)
% ————————————
partido(7,  'Canadá',        'Europa A',           '2026-06-12 16:00', proximo).
partido(8,  'Qatar',         'Suiza',              '2026-06-12 16:00', proximo).
partido(9,  'Suiza',         'Europa A',           '2026-06-18 16:00', proximo).
partido(10, 'Canadá',        'Qatar',              '2026-06-18 19:00', proximo).
partido(11, 'Suiza',         'Canadá',             '2026-06-24 16:00', proximo).
partido(12, 'Europa A',      'Qatar',              '2026-06-24 16:00', proximo).

% ————————————
% Partidos G. C (13–18)
% ————————————
partido(13, 'Brasil',       'Marruecos',          '2026-06-13 19:00', proximo).
partido(14, 'Haití',        'Escocia',            '2026-06-13 22:00', proximo).
partido(15, 'Brasil',       'Haití',              '2026-06-19 19:00', proximo).
partido(16, 'Escocia',      'Marruecos',          '2026-06-19 22:00', proximo).
partido(17, 'Escocia',      'Brasil',             '2026-06-24 19:00', proximo).
partido(18, 'Marruecos',    'Haití',              '2026-06-24 19:00', proximo).

% ————————————
% Partidos G. D (19–24)
% ————————————
partido(19, 'Estados Unidos','Europa C',         '2026-06-12 22:00', proximo).
partido(20, 'Australia',     'Paraguay',          '2026-06-13 01:00', proximo).
partido(21, 'Europa C',      'Paraguay',          '2026-06-19 16:00', proximo).
partido(22, 'Estados Unidos','Australia',         '2026-06-19 01:00', proximo).
partido(23, 'Europa C',      'Estados Unidos',    '2026-06-25 23:00', proximo).
partido(24, 'Paraguay',      'Australia',         '2026-06-25 23:00', proximo).

% ————————————
% Partidos G. E (25–30)
% ————————————
partido(25, 'Alemania',      'Curazao',           '2026-06-14 14:00', proximo).
partido(26, 'Costa de Marfil','Ecuador',          '2026-06-14 20:00', proximo).
partido(27, 'Alemania',      'Costa de Marfil',   '2026-06-20 17:00', proximo).
partido(28, 'Curazao',       'Ecuador',           '2026-06-20 21:00', proximo).
partido(29, 'Ecuador',       'Alemania',          '2026-06-25 17:00', proximo).
partido(30, 'Curazao',       'Costa de Marfil',   '2026-06-25 17:00', proximo).

% ————————————
% Partidos G. F (31–36)
% ————————————
partido(31, 'Países Bajos',  'Japón',             '2026-06-14 17:00', proximo).
partido(32, 'Europa B',      'Túnez',             '2026-06-14 23:00', proximo).
partido(33, 'Países Bajos',  'Europa B',          '2026-06-20 14:00', proximo).
partido(34, 'Japón',         'Túnez',             '2026-06-20 01:00', proximo).
partido(35, 'Túnez',         'Países Bajos',      '2026-06-25 20:00', proximo).
partido(36, 'Japón',         'Europa B',          '2026-06-25 20:00', proximo).

% ————————————
% Partidos G. G (37–42)
% ————————————
partido(37, 'Bélgica',       'Egipto',            '2026-06-15 19:00', proximo).
partido(38, 'Irán',          'Nueva Zelanda',     '2026-06-15 22:00', proximo).
partido(39, 'Bélgica',       'Irán',              '2026-06-21 16:00', proximo).
partido(40, 'Egipto',        'Nueva Zelanda',     '2026-06-21 22:00', proximo).
partido(41, 'Nueva Zelanda','Bélgica',            '2026-06-26 00:00', proximo).
partido(42, 'Egipto',        'Irán',              '2026-06-26 00:00', proximo).

% ————————————
% Partidos G. H (43–48)
% ————————————
partido(43, 'España',        'Cabo Verde',        '2026-06-15 13:00', proximo).
partido(44, 'Arabia Saudita','Uruguay',           '2026-06-15 19:00', proximo).
partido(45, 'España',        'Arabia Saudita',    '2026-06-21 13:00', proximo).
partido(46, 'Cabo Verde',    'Uruguay',           '2026-06-21 19:00', proximo).
partido(47, 'Uruguay',       'España',            '2026-06-26 21:00', proximo).
partido(48, 'Cabo Verde',    'Arabia Saudita',    '2026-06-26 21:00', proximo).

% ————————————
% Partidos G. I (49–54)
% ————————————
partido(49, 'Francia',       'Senegal',           '2026-06-16 16:00', proximo).
partido(50, 'Repechaje 2',   'Noruega',           '2026-06-16 19:00', proximo).
partido(51, 'Francia',       'Repechaje 2',       '2026-06-22 18:00', proximo).
partido(52, 'Noruega',       'Senegal',           '2026-06-22 21:00', proximo).
partido(53, 'Noruega',       'Francia',           '2026-06-26 16:00', proximo).
partido(54, 'Senegal',       'Repechaje 2',       '2026-06-26 16:00', proximo).

% ————————————
% Partidos G. J (55–60)
% ————————————
partido(55, 'Argentina',     'Argelia',           '2026-06-16 22:00', proximo).
partido(56, 'Austria',       'Jordania',          '2026-06-17 01:00', proximo).
partido(57, 'Argentina',     'Austria',           '2026-06-22 14:00', proximo).
partido(58, 'Jordania',      'Argelia',           '2026-06-22 00:00', proximo).
partido(59, 'Jordania',      'Argentina',         '2026-06-27 23:00', proximo).
partido(60, 'Argelia',       'Austria',           '2026-06-27 23:00', proximo).

% ————————————
% Partidos G. K (61–66)
% ————————————
partido(61, 'Portugal',      'Repechaje 1',       '2026-06-17 14:00', proximo).
partido(62, 'Uzbekistán',    'Colombia',          '2026-06-17 23:00', proximo).
partido(63, 'Portugal',      'Uzbekistán',        '2026-06-23 14:00', proximo).
partido(64, 'Repechaje 1',   'Colombia',          '2026-06-23 23:00', proximo).
partido(65, 'Colombia',      'Portugal',          '2026-06-27 20:30', proximo).
partido(66, 'Repechaje 1',   'Uzbekistán',        '2026-06-27 20:30', proximo).

% ————————————
% Partidos G. L (67–72)
% ————————————
partido(67, 'Inglaterra',    'Croacia',           '2026-06-17 17:00', proximo).
partido(68, 'Ghana',         'Panamá',            '2026-06-17 20:00', proximo).
partido(69, 'Inglaterra',    'Ghana',             '2026-06-23 17:00', proximo).
partido(70, 'Croacia',       'Panamá',            '2026-06-23 20:00', proximo).
partido(71, 'Panamá',        'Inglaterra',        '2026-06-27 18:00', proximo).
partido(72, 'Croacia',       'Ghana',             '2026-06-27 18:00', proximo).

%%% ––––––––––––––––––––––––––––––––––––––––––––––––
%%% Hechos partido/5 para eliminación directa
%%% ––––––––––––––––––––––––––––––––––––––––––––––––
% 16avos de final (IDs 73–80 según tu txt)
partido(73, ganador('A'), subcampeon('B'), '2026-06-28 16:00', proximo).
partido(74, ganador('C'), subcampeon('F'), '2026-06-28 17:30', proximo).
partido(75, ganador('E'), mejor_tercero(1), '2026-06-28 22:00', proximo).
partido(76, ganador('F'), subcampeon('C'), '2026-06-29 14:00', proximo).
partido(77, subcampeon('E'), mejor_tercero(2), '2026-06-30 18:00', proximo).
partido(78, subcampeon('A'), mejor_tercero(3), '2026-06-30 14:00', proximo).
partido(79, ganador('B'), mejor_tercero(4), '2026-06-30 22:00', proximo).
partido(80, ganador('L'), mejor_tercero(5), '2026-07-01 13:00', proximo).

% Octavos de final (IDs 89–96)
partido(89, ganador_partido(73), ganador_partido(75), '2026-07-04 18:00', proximo).
partido(90, ganador_partido(74), ganador_partido(77), '2026-07-04 14:00', proximo).
partido(91, ganador_partido(76), ganador_partido(78), '2026-07-05 17:00', proximo).
partido(92, ganador_partido(79), ganador_partido(80), '2026-07-05 21:00', proximo).
partido(93, ganador_partido(83), ganador_partido(84), '2026-07-06 16:00', proximo).
partido(94, ganador_partido(81), ganador_partido(82), '2026-07-06 21:00', proximo).
partido(95, ganador_partido(86), ganador_partido(88), '2026-07-07 13:00', proximo).
partido(96, ganador_partido(85), ganador_partido(87), '2026-07-07 17:00', proximo).

% Cuartos de final (IDs 97–100)
partido(97, ganador_partido(89), ganador_partido(90), '2026-07-09 17:00', proximo).
partido(98, ganador_partido(93), ganador_partido(94), '2026-07-10 16:00', proximo).
partido(99, ganador_partido(91), ganador_partido(92), '2026-07-11 18:00', proximo).
partido(100,ganador_partido(95), ganador_partido(96), '2026-07-11 22:00', proximo).

% Semifinales (IDs 101–102)
partido(101, ganador_partido(97), ganador_partido(98), '2026-07-14 16:00', proximo).
partido(102, ganador_partido(99), ganador_partido(100),'2026-07-15 16:00', proximo).

% Tercer puesto y Final
partido(103, perdedor_partido(101), perdedor_partido(102), '2026-07-18 18:00', proximo).
partido(104, ganador_partido(101), ganador_partido(102), '2026-07-19 16:00', proximo).

%%% ––––––––––––––––––––––––––––––––––––––––––––––––
%%% Utils para eliminación directa
%%% ––––––––––––––––––––––––––––––––––––––––––––––––
% Recolecta tercer lugar de cada grupo y ordena para mejores 4
mejores_terceros(Terceros) :-
  grupos(Grs),
  findall([Pts,GD,GF,Equipo],
    ( member(G, Grs),
      findall([RGA,RGB,Res],
        ( partido(Id,A,B,_,jugado), resultado(Id,GA-GB),
          grupo(A,G), A==Equipo0, Equipo=Equipo0,
          RGA=GA, RGB=GB,
          (RGA>RGB->Res=win;RGA<RGB->Res=loss;Res=tie)
        ),
      Part),
      estadisticas_orden(Part, [_,_,Equipo|_] ),  % tercera posición
      estadisticas(Equipo,[_,_,_,_,GF,_,GD,Pts])
    ),
  L),
  sort(1,@>=,L,Desc),
  findall(Eq, nth1(_,Desc, [_,_,_,Eq]), TercerosUnsorted),
  firstn(4,TercerosUnsorted,Terceros).

% Helpers para ordenar Pts, GD, GF y extraer Equipo
estadisticas_orden(Partidos, Orden) :-
  findall([Pts,GD,GF,Equipo],
    ( member([RGA,RGB,_],Partidos),
      Estad = [PJ,PG,PE,PP,GF,GC,GD,Pts], estadisticas(Equipo,Estad)
    ), Datos),
  sort(1,@>=,Datos,Orden).

firstn(_,[],[]).
firstn(N,[X|Xs],[X|Ys]) :- N>0, N1 is N-1, firstn(N1,Xs,Ys).

% Resolver placeholders de equipos
resolver_equipo(ganador(G), Equipo) :-
  setof([P,GD,GF,Eq], ( grupo(Eq,G), estadisticas(Eq,[_ ,_,_,_,GF,_,GD,P]) ), D),
  last(D, [_,_,_,Equipo]).  % ganador = mayor Pts/GD/GF

resolver_equipo(subcampeon(G), Equipo) :-
  setof([P,GD,GF,Eq], ( grupo(Eq,G), estadisticas(Eq,[_ ,_,_,_,GF,_,GD,P]) ), D),
  nth1(2, D, [_,_,_,Equipo]).

resolver_equipo(mejor_tercero(N), Equipo) :-
  mejores_terceros(T), nth1(N,T,Equipo).

resolver_equipo(ganador_partido(Id),Equipo) :-
  partido(Id,A,B,_,jugado), resultado(Id,GA-GB),
  ( GA>GB -> Equipo=A ; Equipo=B ).

resolver_equipo(perdedor_partido(Id),Equipo) :-
  partido(Id,A,B,_,jugado), resultado(Id,GA-GB),
  ( GA>GB -> Equipo=B ; Equipo=A ).

% Nombre final de un término (si ya es átomo, queda igual)
nombre_equipo(E,N) :- atom(E), !, N=E.
nombre_equipo(E,N) :- resolver_equipo(E,N).

% punto de entrada que llama a http_server/2
inicio :-
    http_server(http_dispatch, [port(8080)]).

:- http_handler(root(.),           mostrar_fixture,   []).
:- http_handler(root(resultar),    agregar_resultado, [method(post)]).
:- http_handler(root(resultados),  mostrar_resultados,[]).
:- http_handler(root(tabla),       mostrar_tabla,     []).

mostrar_fixture(_Req) :-
    findall(html(tr([ td(ID),
                     td(NomA),
                     td(NomB),
                     td(Fecha)
                   ])),
            ( partido(ID, EA, EB, Fecha, proximo),
              nombre_equipo(EA, NomA),
              nombre_equipo(EB, NomB)
            ),
            Filas),
    reply_html_page(
      [ title('Fixture Mundial 2026'),
        link([rel('stylesheet'), href('/static/style.css')])
      ],
      [ nav([ a(href('/'),        'Próximos'),
              a(href('/resultados'),'Resultados'),
              a(href('/tabla'),     'Tabla de Posiciones')
            ]),
        h1('Próximos Partidos'),
        table([],
              [ tr([th('ID'),th('Equipo A'),th('Equipo B'),th('Fecha')]) | Filas ]),
        form([action('/resultar'),method('POST'), class('result-form')],
             [ label([for(id)], 'ID partido:'), input([name(id),type(text)]), br([]),
               label([for(gola)], 'Goles Equipo A:'), input([name(gola),type(text)]), br([]),
               label([for(golb)], 'Goles Equipo B:'), input([name(golb),type(text)]), br([]),
               input([type(submit),value('Cargar Resultado')])
             ])
      ]).

agregar_resultado(Request) :-
    http_read_data(Request, Params, []),
    memberchk(id=IdAtom, Params), atom_number(IdAtom, Id),
    memberchk(gola=GAAtom, Params), atom_number(GAAtom, GA),
    memberchk(golb=GBAtom, Params), atom_number(GBAtom, GB),
    retract(partido(Id, A, B, Fecha, proximo)),    % marcamos como jugado
    assertz(partido(Id, A, B, Fecha, jugado)),
    assertz(resultado(Id, GA-GB)),
    reply_html_page(
      [ title('Resultado Cargado'),
        link([rel('stylesheet'), href('/static/style.css')])
      ],
      [ nav([ a(href('/'),        'Próximos'),
              a(href('/resultados'),'Resultados'),
              a(href('/tabla'),     'Tabla de Posiciones')
            ]),
        h1('¡Resultado registrado!'),
        p(['Partido ', Id, ': ', A, ' ', GA, ' - ', GB, ' ', B]),
        a(href('/'), 'Volver al fixture')
      ]).
mostrar_resultados(_Req) :-
  findall(html(tr([ td(ID),
                   td(A), td(B),
                   td(GA), td(GB),
                   td(Fecha)
                 ])),
          ( partido(ID,A,B,Fecha,jugado),
            resultado(ID,GA-GB)
          ),
          Filas),
  reply_html_page(
    [ title('Resultados Mundial 2026'),
      link([rel('stylesheet'), href('/static/style.css')])
    ],
    [ nav([ a(href('/'),        'Próximos'),
            a(href('/resultados'),'Resultados'),
            a(href('/tabla'),     'Tabla de Posiciones')
          ]),
      h1('Resultados Registrados'),
      table([], [ tr([th('ID'),th('A'),th('B'),th('Goles A'),th('Goles B'),th('Fecha')])|Filas ])
    ]).

% Lista de todos los grupos
grupos(Lista) :-
  setof(G, Equipo^grupo(Equipo,G), Lista).

% Estadísticas [PJ,PG,PE,PP,GF,GC,GD,Pts] de un Equipo
estadisticas(Equipo,[PJ,PG,PE,PP,GF,GC,GD,Pts]) :-
  findall([RGA,RGB,Res],
    ( partido(Id,A,B,_,jugado),
      resultado(Id,GA-GB),
      ( A==Equipo -> RGA=GA, RGB=GB ; B==Equipo -> RGA=GB, RGB=GA ),
      ( RGA>RGB -> Res=win
      ; RGA<RGB -> Res=loss
      ; Res=tie
      )
    ),
  Partidos),
  length(Partidos,PJ),
  include(win,  Partidos,W), length(W,PG),
  include(tie,  Partidos,T), length(T,PE),
  include(loss, Partidos,L), length(L,PP),
  findall(X, member([X,_,_],Partidos), L1), sum_list(L1,GF),
  findall(Y, member([_,Y,_],Partidos), L2), sum_list(L2,GC),
  GD is GF-GC,
  Pts is PG*3 + PE.

win([_,_,win]).
tie([_,_,tie]).
loss([_,_,loss]).


% DCG que genera la tabla de posiciones de cada grupo
generar_tablas([]) --> [].
generar_tablas([G|Gs]) -->
  { setof(E, grupo(E,G), Equipos),
    findall([Pts,GD,Equipo,PJ,PG,PE,PP,GF,GC],
      ( member(Equipo,Equipos),
        estadisticas(Equipo,[PJ,PG,PE,PP,GF,GC,GD,Pts])
      ),
    Datos),
    sort(1,@>=,Datos,Ordenado)
  },
  html([
    h2(['Grupo ', G]),
    table([class('tabla-grupo')],
      [ tr([ th('Equipo'),th('PJ'),th('PG'),th('PE'),th('PP'),
            th('GF'),th('GC'),th('DG'),th('Pts') ])
      | \filas_estadisticas(Ordenado)
      ]
    ),
    br([])
  ]),
  generar_tablas(Gs).

filas_estadisticas([]) --> [].
filas_estadisticas([[Pts,GD,Equipo,PJ,PG,PE,PP,GF,GC]|Rest]) -->
  html(tr([ td(Equipo), td(PJ), td(PG), td(PE),
            td(PP), td(GF), td(GC), td(GD), td(Pts) ])),
  filas_estadisticas(Rest).

mostrar_tabla(_Req) :-
  grupos(Grps),
  reply_html_page(
    [ title('Tabla de Posiciones'),
      link([rel('stylesheet'), href('/static/style.css')])
    ],
    [ nav([ a(href('/'),        'Próximos'),
            a(href('/resultados'),'Resultados'),
            a(href('/tabla'),     'Tabla de Posiciones')
          ]),
      h1('Tabla de Posiciones por Grupo'),
      \generar_tablas(Grps)
    ]).
