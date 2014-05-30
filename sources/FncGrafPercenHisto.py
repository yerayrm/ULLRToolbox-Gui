from PySide import QtCore, QtGui
from rpy2 import rinterface, robjects
import sys

class FncGrafPercenHisto():

	#Constructor
	def __init__(self, ui, d_percentiles, d_histograma):
		self.ui = ui
		self.d_percentiles = d_percentiles
		self.d_histograma = d_histograma

		QtCore.QObject.connect(self.ui.act_percentiles, QtCore.SIGNAL("triggered()"), self.openPercentiles)
		QtCore.QObject.connect(self.ui.act_histogramas, QtCore.SIGNAL("triggered()"), self.openHistograma)
		


	def openPercentiles(self):
		self.dialogUi = self.d_percentiles
		self.dialogUi.setWindowTitle("Percentiles")
		self.dialogUi.show()


	def openHistograma(self):
		self.dialogUi = self.d_histograma
		self.dialogUi.setWindowTitle("Histograma")
		self.dialogUi.show()