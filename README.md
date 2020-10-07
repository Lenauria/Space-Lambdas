# Space-Lambdas

Zur Ausführung des Programms wird Stack benötigt.
https://docs.haskellstack.org/en/stable/README/

Ausführung unseres Projekts:
stack build
stack run

Abhängigkeiten unter Debian:
zlib1g-dev libgl-dev libglu-dev libglfw3-dev freeglut3-dev

Bei der Verwendung von Windows können in Zusammenhang mit OpenGL Probleme autreten. Speziell tritt der Fehler "user error (unknown GLUT entry glutInit)" gehäuft auf. Der Fehler besteht meistens darin, dass die glut32.dll fehlt. Das Problem konnte auf unseren Rechnern durch folgenden Lösungsvorschlag behoben werden:
https://stackoverflow.com/a/32808872
