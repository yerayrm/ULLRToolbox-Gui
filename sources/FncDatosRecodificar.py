from PySide import QtCore, QtGui
from rpy2 import rinterface, robjects
import sys

class FncDatosRecodificar():

	#Constructor
	def __init__(self, ui, d_recodificar):
		self.ui = ui
		self.d_recodificar = d_recodificar

		QtCore.QObject.connect(self.ui.act_recodificar, QtCore.SIGNAL("triggered()"), self.openRecodificar)
		


	def openRecodificar(self):
		self.dialogUi = self.d_recodificar
		self.dialogUi.setWindowTitle("Recodificar")
		self.dialogUi.show()


