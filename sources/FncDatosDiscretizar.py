from PySide import QtCore, QtGui
from rpy2 import rinterface, robjects
import sys

class FncDatosDiscretizar():

	#Constructor
	def __init__(self, ui, d_discretizar):
		self.ui = ui
		self.d_discretizar = d_discretizar

		QtCore.QObject.connect(self.ui.act_discretizar, QtCore.SIGNAL("triggered()"), self.openDiscretizar)
		


	def openDiscretizar(self):
		# Abro el dialogo
		self.dialogUi = self.d_discretizar
		self.dialogUi.setWindowTitle("Discretizar")
		self.dialogUi.show()

		# Inicializo los combobox
		def f(x):
			print x
		rinterface.set_writeconsole(f)
		n_items = "length(names(datos))"
		n_items = robjects.r(n_items)
		n_items = n_items[0]
		self.dialogUi.cb_discretizar_1.clear()
		self.dialogUi.cb_discretizar_2.clear()
		self.dialogUi.cb_discretizar_2.addItem(str("Sin factor"))
		for i in range(n_items):
			item_factor = "names(datos)[" + str(i+1) + "]"
			item_factor = robjects.r(item_factor)
			self.dialogUi.cb_discretizar_1.addItem(str(item_factor[0]))
			self.dialogUi.cb_discretizar_2.addItem(str(item_factor[0]))

		rinterface.set_writeconsole(rinterface.consolePrint)

		# Senyales de aceptar y cancelar
		QtCore.QObject.connect(self.dialogUi.buttonBox, QtCore.SIGNAL("accepted()"), self.accept, QtCore.Qt.UniqueConnection)
		QtCore.QObject.connect(self.dialogUi.buttonBox, QtCore.SIGNAL("rejected()"), self.cancel, QtCore.Qt.UniqueConnection)





	def accept(self):
		print ("*Accept*")
		variable = str(self.dialogUi.cb_discretizar_1.currentText())
		factor = str(self.dialogUi.cb_discretizar_2.currentText())

		if self.dialogUi.rb_cuartil.isChecked():
			ntiles = "4"
		elif self.dialogUi.rb_decil.isChecked():
			ntiles = "10"
		elif self.dialogUi.rb_percentil.isChecked():
			ntiles = "100"
		else:
			self.openAgregadoDialog()

		if (factor == "Sin factor"):
			comando = "datos.discretizados=discretiza.variable.fnc(datos, variable='" + variable + "',ntiles=" + ntiles + ")"
		else:
			comando = "datos.discretizados=discretiza.variable.fnc(datos, variable='" + variable + "', que.factor='" + factor + "',ntiles=" + ntiles + ")"


		self.ui.text_result.append("> " + comando)

		def f(x):
			self.ui.text_result.textCursor().insertText(x)

		rinterface.set_writeconsole(f)
		resultado = robjects.r(comando)
		self.ui.text_result.append(str(resultado))
		
		rinterface.set_writeconsole(rinterface.consolePrint)



	def cancel(self):
		print ("*Cancel*")