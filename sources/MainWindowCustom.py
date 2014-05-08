from PySide import QtCore, QtGui
from rpy2 import rinterface, robjects
import sys
import interface.mainwindow as mw
import key_press_eater as kp

class MainWindowCustom():

	#Constructor
	def __init__(self, ui):
		self.ui = ui

	def onCreate(self):
		# actions
		self.act_abrir = QtGui.QAction(QtGui.QIcon('./resources/openIcon.png'), 'Abrir archivo', self.ui)
		self.act_guardar = QtGui.QAction(QtGui.QIcon('./resources/saveIcon.png'), "Guardar Archivo", self.ui)
		self.act_cerrar = QtGui.QAction("Cerrar", self.ui)

		# # toolBar
		self.ui.toolBar.addAction(self.act_abrir)
		self.ui.toolBar.addAction(self.act_guardar)

		self.ui.mnu_archivo.addAction(self.act_abrir)
		self.ui.mnu_archivo.addAction(self.act_guardar)
		self.ui.mnu_archivo.addSeparator()
		self.ui.mnu_archivo.addAction(self.act_cerrar)

		# # signals
		#QtCore.QObject.connect(self.act_abrir, QtCore.SIGNAL("triggered()"), self.abrirArchivo)
		#QtCore.QObject.connect(self.button_ejecutar, QtCore.SIGNAL("clicked()"), self.insertCommand)
		QtCore.QObject.connect(self.act_cerrar, QtCore.SIGNAL("triggered()"), self.ui.close)

