from PySide import QtCore, QtGui
from rpy2 import rinterface, robjects
import sys

class FncDatosDiscretizar():

	#Constructor
	def __init__(self, ui):
		self.ui = ui
		self.d_discretizar = d_discretizar

		QtCore.QObject.connect(self.ui.act_discretizar, QtCore.SIGNAL("triggered()"), self.openDiscretizar)
		


	def openDiscretizar(self):
		print ""


