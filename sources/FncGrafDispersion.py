from PySide import QtCore, QtGui
from rpy2 import rinterface, robjects
import sys

class FncGrafDispersion():

	#Constructor
	def __init__(self, ui, d_dispersion):
		self.ui = ui
		self.d_dispersion = d_dispersion

		QtCore.QObject.connect(self.ui.act_dispersion, QtCore.SIGNAL("triggered()"), self.openDispersion)
		


	def openDispersion(self):
		self.dialogUi = self.d_dispersion
		self.dialogUi.setWindowTitle("Grafica de dispersion")
		self.dialogUi.show()

