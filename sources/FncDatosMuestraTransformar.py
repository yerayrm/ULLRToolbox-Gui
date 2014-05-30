from PySide import QtCore, QtGui
from rpy2 import rinterface, robjects
import sys

class FncDatosMuestraTransformar():

	#Constructor
	def __init__(self, ui, d_muestra, d_transformar):
		self.ui = ui
		self.d_muestra = d_muestra
		self.d_transformar = d_transformar

		QtCore.QObject.connect(self.ui.act_extraer_mues, QtCore.SIGNAL("triggered()"), self.openMuestra)
		QtCore.QObject.connect(self.ui.act_transformar, QtCore.SIGNAL("triggered()"), self.openTransformar)
		


	def openMuestra(self):
		self.dialogUi = self.d_muestra
		self.dialogUi.setWindowTitle("Extraer muestra")
		self.dialogUi.show()


	def openTransformar(self):
		self.dialogUi = self.d_transformar
		self.dialogUi.setWindowTitle("Transformar")
		self.dialogUi.show()


