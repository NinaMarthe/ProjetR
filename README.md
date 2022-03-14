# ProjetR

Bonjour ! Voici une brève description de notre projet R de l’UE HAX815X Ce projet a pour but de générer aléatoirement des grilles de Sudoku complètes et incomplètes associées à un niveau de difficulté , ainsi que de résoudre des grilles de Sudoku incomplètes

La fonction "grille_complete()" génère une grille de sudoku aléatoire complète. \n
la fonction "grille_incomplete(n)" avec n de 1 à 3 génère une grille de sudoku incomplète aléatoire,
la valeur dans n représentant la difficulté. \n
La fonction "solveur(s)" résoud la grille de sudoku s et renvoie sa solution. \n

authors = ("Nina MARTHE", "Elyes BRAHAM")

contact = ("nina.marthe@etu.umontpellier.fr","elyes.braham@etu.umontpellier.fr")

version = "0.0.1"

date = "14/03/2022"

licence ="This program is free software: you can redistribute it and/or modify" under the terms of the GNU General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details. You should have received a copy of the GNU General Public License along with this program. If not, see https://www.gnu.org/licenses/.


Installer le package :

library(devtools)

devtools::install_github("NinaMarthe/ProjetR")
