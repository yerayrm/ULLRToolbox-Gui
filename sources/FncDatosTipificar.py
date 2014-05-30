from PySide import QtCore, QtGui
from rpy2 import rinterface, robjects
import sys

class FncDatosTipificar():

	#Constructor
	def __init__(self, ui, d_tipificar):
		self.ui = ui
		self.d_tipificar = d_tipificar

		QtCore.QObject.connect(self.ui.act_tipificar, QtCore.SIGNAL("triggered()"), self.openTipificar)
		


	def openTipificar(self):
		self.dialogUi = self.d_tipificar
		self.dialogUi.setWindowTitle("Tipificar")
		self.dialogUi.show()


