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
		

	######################################################################################################
	### EXTRAER MUESTRA
	def openMuestra(self):
		self.dialogUi = self.d_muestra
		self.dialogUi.setWindowTitle("Extraer muestra")
		self.dialogUi.show()

		# Inicializo los combobox
		def f(x):
			print x
		rinterface.set_writeconsole(f)
		n_items = "length(names(datos))"
		n_items = robjects.r(n_items)
		n_items = n_items[0]
		self.dialogUi.cb_factor.clear()
		self.dialogUi.cb_factor.addItem(str("Sin factor"))
		for i in range(n_items):
			item_factor = "names(datos)[" + str(i+1) + "]"
			item_factor = robjects.r(item_factor)
			self.dialogUi.cb_factor.addItem(str(item_factor[0]))

		rinterface.set_writeconsole(rinterface.consolePrint)

		# Senyales de aceptar y cancelar
		QtCore.QObject.connect(self.dialogUi.buttonBox, QtCore.SIGNAL("accepted()"), self.acceptMuestra, QtCore.Qt.UniqueConnection)
		QtCore.QObject.connect(self.dialogUi.buttonBox, QtCore.SIGNAL("rejected()"), self.cancel, QtCore.Qt.UniqueConnection)


	def acceptMuestra(self):
		print ("*Accept*")
		identificacion = self.dialogUi.id_text.text()
		n_casos = self.dialogUi.n_text.text()
		factor = str(self.dialogUi.cb_factor.currentText())

		if (factor == "Sin factor"):
			comando = "muestra = extrae.muestra.fnc(datos, n=" + n_casos + ", ID='" + identificacion+ "')"
		else:
			comando = "muestra = extrae.muestra.fnc(datos, que.factor='" + factor + "', n=" + n_casos + ", ID='" + identificacion+ "')"
		

		self.ui.text_result.append("> " + comando)

		def f(x):
			self.ui.text_result.textCursor().insertText(x)

		rinterface.set_writeconsole(f)
		resultado = robjects.r(comando)
		self.ui.text_result.append(str(resultado))
		
		rinterface.set_writeconsole(rinterface.consolePrint)



	def cancel(self):
		print ("*Cancel*")



	######################################################################################################
	### TRANSFORMAR LA VARIABLE
	def openTransformar(self):
		self.dialogUi = self.d_transformar
		self.dialogUi.setWindowTitle("Transformar")
		self.dialogUi.show()


