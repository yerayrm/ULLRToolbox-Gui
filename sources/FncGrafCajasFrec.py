from PySide import QtCore, QtGui
from rpy2 import rinterface, robjects
import sys

class FncGrafCajasFrec():

	#Constructor
	def __init__(self, ui, d_cajas, d_frec):
		self.ui = ui
		self.d_cajas = d_cajas
		self.d_frec = d_frec

		QtCore.QObject.connect(self.ui.act_cajas, QtCore.SIGNAL("triggered()"), self.openCajas)
		QtCore.QObject.connect(self.ui.act_frecuencias, QtCore.SIGNAL("triggered()"), self.openFrecuencias)
		


	def openCajas(self):
		self.dialogUi = self.d_cajas
		self.dialogUi.setWindowTitle("Diagrama de cajas")
		self.dialogUi.show()


	def openFrecuencias(self):
		self.dialogUi = self.d_frec
		self.dialogUi.setWindowTitle("Frecuencias")
		self.dialogUi.show()
