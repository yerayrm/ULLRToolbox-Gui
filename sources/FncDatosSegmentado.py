from PySide import QtCore, QtGui
from rpy2 import rinterface, robjects
import sys

class FncDatosSegmentado():

	#Constructor
	def __init__(self, ui, d_segmentado):
		self.ui = ui
		self.d_segmentado = d_segmentado

		QtCore.QObject.connect(self.ui.act_segmentado, QtCore.SIGNAL("triggered()"), self.openSegmentadoDialog)



	def openSegmentadoDialog(self):
		self.dSegmentadoUi = self.d_segmentado
		self.dSegmentadoUi.setWindowTitle("Segmentado")
		self.dSegmentadoUi.show()
		## cargo los elementos del combobox
		def f(x):
			print x
		rinterface.set_writeconsole(f)
		n_items = "length(names(datos))"
		n_items = robjects.r(n_items)
		n_items = n_items[0]
		self.dSegmentadoUi.factorCombo.clear()
		for i in range(n_items):
			item_factor = "names(datos)[" + str(i+1) + "]"
			item_factor = robjects.r(item_factor)
			self.dSegmentadoUi.factorCombo.addItem(str(item_factor[0]))

		rinterface.set_writeconsole(rinterface.consolePrint)
 
		## senales
		self.dSegmentadoUi.buttonBox.accepted.connect(self.acceptSegmentado)
		self.dSegmentadoUi.buttonBox.rejected.connect(self.cancel)



	def acceptSegmentado(self):
		print ("*Accept*")
		comando = "x.factor=divide.por.factor.fnc(datos, que.factor='" + str(self.dSegmentadoUi.factorCombo.currentText()) + "')"
		self.ui.text_result.append("> " + comando)
		def f(x):
			print x
		rinterface.set_writeconsole(f)
		resultado = robjects.r(comando)
		self.ui.text_result.append(str(resultado))
		rinterface.set_writeconsole(rinterface.consolePrint)



	def cancel(self):
		print ("*Cancel*")



