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


%serve_files_in_directory(Dir, Request) :-
%		http_reply_from_files(Dir, [], Request).

:- dynamic partido/5.    % partido(ID, EquipoA, EquipoB, FechaHora, Estado)
:- dynamic resultado/2.   % resultado(ID, GolesA-GolesB)

:- dynamic grupo/2.
grupo(argentina,    grupoA).  grupo(arabiasaudita, grupoA).
grupo(brasil,       grupoB).  grupo(coreadelsur,  grupoB).
grupo(españa,       grupoC).  grupo(japon,        grupoC).
grupo(alemania,     grupoD).  grupo(méxico,       grupoD).

% partido(ID, EquipoA, EquipoB, FechaHora, Estado)
% Estado = próximo | jugado
partido(1, argentina, arabiasaudita, '2026-06-10 18:00', proximo).
partido(2, brasil, coreadelsur,    '2026-06-11 21:00', proximo).
partido(3, españa, japon,         '2026-06-12 18:00', proximo).
partido(4, alemania, méxico,      '2026-06-13 21:00', proximo).

% punto de entrada que llama a http_server/2
inicio :-
    http_server(http_dispatch, [port(8080)]).

:- http_handler(root(.),           mostrar_fixture,   []).
:- http_handler(root(resultar),    agregar_resultado, [method(post)]).
:- http_handler(root(resultados),  mostrar_resultados,[]).
:- http_handler(root(tabla),       mostrar_tabla,     []).

mostrar_fixture(_Req) :-
    findall(html(tr([ td(ID), td(EquipoA), td(EquipoB), td(Fecha) ])),
            ( partido(ID, EquipoA, EquipoB, Fecha, proximo) ),
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
        form([action('/resultar'),method('POST')],
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
  findall(html(tr([td(ID),td(A),td(B),td(GA),td(GB),td(Fecha)])),
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

mostrar_tabla(_Req) :-
  % … aquí pones tu lógica de grupos y estadisticas …
  reply_html_page(
    [ title('Tabla de Posiciones'),
      link([rel('stylesheet'), href('/static/style.css')])
    ],
    [ nav([ a(href('/'),        'Próximos'),
            a(href('/resultados'),'Resultados'),
            a(href('/tabla'),     'Tabla de Posiciones')
          ]),
      h1('Tabla de Posiciones por Grupo')
      % \listas_de_tablas(…) 
    ]).
