# nonograms
Los  nonogramas son un tipo de rompecabezas de lógica en el que se debe rellenar una cuadrícula siguiendo las pistas numéricas que aparecen en cada fila y columna.
- Los números indican cuántos bloques consecutivos deben estar pintados.
- El objetivo es descubrir una imagen oculta al completar correctamente la cuadrícula.
- Se resuelven aplicando razonamiento lógico, descartando casillas y validando patrones.

Este proyecto está desarrollado en Haskell, que actualmente cuenta con 6 niveles de dificultad.

# Funcionalidades principales:
- Rellenar un cuadrado con gris si debe estar pintado.
- Marcar con una cruz (X) si no debe estar pintado.
- La aplicación te indica si una casilla marcada (gris o cruz) incumple las reglas del nonograma.
- Al completar correctamente las reglas de una fila o columna, las casillas blancas restantes se marcan automáticamente con cruces.
- El sistema te avisa cuando has terminado de resolver el nivel.

# Funcionalidades pendientes:
- Implementar un algoritmo que pueda resolver cualquier nonograma de forma automática o de pistas cuando lo pida el usuario.
