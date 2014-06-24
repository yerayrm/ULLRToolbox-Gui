# GUI de ULLRToolbox para R


## Introducción
Los programas informáticos se han convertido, hoy en día, en una herramienta básica utilizada por el análisis estadístico como apoyo fundamental a la hora de realizar diferentes operaciones y para facilitar una mayor comodidad a los usuarios. Actualmente, R es uno de los programas más utilizados debido a su potencia y a su distribución como software libre. Sin embargo, cuenta con dos grandes inconvenientes: tiene una sintaxis compleja y no dispone de interfaz gráfica que permita ser usable por aquellos que no son informáticos. Para solucionar el primer inconveniente, el Departamento de Psicobiologia y Metodología de la Universidad de La Laguna ha creado ULLRToolbox, un script en el que se pueden llamar a distintas funciones de análisis estadístico de forma simple. Este proyecto pretende solventar el segundo inconveniente creando una interfaz gráfica que permita llamar mediante menús a las distintas funciones del script ULLRToolbox haciendo uso de la biblioteca Qt y en concreto un binding de éste llamado PySide.


## Estructura del proyecto

![alt text](http://s13.postimg.org/pg21uvhdz/estructura.png "Logo Title Text 1")

* La carpeta **build** contiene el proyecto construido para los diferentes sistemas operativos. Por ahora
contiene dos subcarpetas que contiene los archivos ejecutables para linux de 32 bits y
para 64 bits, aunque también puede construirse para Windows u otros sistemas modificando el archivo setup.py.

* La carpeta **forms** contiene todos las interfaces. Una lista de 29 archivos entre los cuales se incluye la ventana del proyecto (mainwindow.ui), mientras que el resto son cada uno de los diálogos
asociados a cada función del programa.

* La carpeta **resources** contiene todas las imágenes que se usan en el programa. Estas imágenes son, básicamente, el icono del programa y los iconos de la barra de tareas.

* En la carpeta **samples**, tal y como su nombre indica, se encuentran una lista de ejemplos de archivos para que el usuario pueda probar las funciones disponibles en la documentación de ULLRToolbox sin necesidad de estar descargándoselos.

* En la carpeta **script** se encuentra el archivo escrito en R donde están programadas todas las funciones que componen la caja de herramientas creadas por los desarrolladores de ULLRToolbox. Esta carpeta permite que si los programadores del toolbox actualizan su script, en cualquier momento el usuario puede descargárselo e incorporarlo a esta carpeta sin necesidad de descargarse de nuevo el GUI. Automáticamente, al iniciar el programa gráfico este hace una comprobación y pide al usuario que instale, si las hay, las nuevas librerías.

* La carpeta **sources** contiene todos los ficheros Python que permiten darle funcionalidad a la interfaz gráfica del proyecto, tanto los diálogos como la ventana principal.



## Instalación

Ir a la carpeta buid, seleccionar la carpeta dependiendo del SO (x86 para 32 bits o x64 para 64 bits) y ejecutar el ejecutable llamado 'main'.

[En construcción]


## Colaboradores

* Alumno - Domingo Yeray Rodríguez Martín
* Director del proyecto - Jesus Miguel Torres Jorge




