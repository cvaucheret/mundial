# Fixture Mundial 2026

Este proyecto es una aplicación web en Prolog (SWI-Prolog) que permite gestionar el fixture, cargar resultados y visualizar la tabla de posiciones de un torneo de fútbol.

## Requisitos

- [SWI-Prolog](https://www.swi-prolog.org/) (recomendado 8.0 o superior)
- Navegador web moderno

## Instalación

1. Clona o descarga este repositorio.
2. Asegúrate de tener SWI-Prolog instalado.

## Ejecución

Desde la terminal, en la carpeta del proyecto, ejecuta:

```sh
swipl fixture.pl
```

Luego, en la consola de Prolog, inicia el servidor con:

```prolog
?- inicio.
```

Esto levantará el servidor en [http://localhost:8080](http://localhost:8080).

## Uso

- Accede a [http://localhost:8080](http://localhost:8080) para ver los próximos partidos.
- Puedes cargar resultados usando el formulario en la página principal.
- Consulta los resultados y la tabla de posiciones desde la barra de navegación.

## Estructura

- `fixture.pl`: Código fuente principal de la aplicación.
- `static/style.css`: Hojas de estilo CSS.
- `README.md`: Este archivo.
- `.gitignore`: Ignora archivos temporales y de backup de Prolog.

## Dependencias

- SWI-Prolog y sus librerías estándar (`http/thread_httpd`, `http/html_write`, etc.).

## Notas

- Los datos se almacenan en memoria. Al reiniciar el servidor, se pierden los resultados cargados.
- Puedes modificar los equipos y partidos editando los hechos en `fixture.pl`.
