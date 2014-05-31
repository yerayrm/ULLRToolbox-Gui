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
		# Abro el dialogo
		self.dialogUi = self.d_recodificar
		self.dialogUi.setWindowTitle("Recodificar")
		self.dialogUi.show()

		# Inicializo los combobox
		def f(x):
			print x
		rinterface.set_writeconsole(f)
		n_items = "length(names(datos))"
		n_items = robjects.r(n_items)
		n_items = n_items[0]
		self.dialogUi.cb_variable_reco.clear()
		self.dialogUi.cb_variable_reco.addItem("**Variable no seleccionada**")
		for i in range(n_items):
			item_factor = "names(datos)[" + str(i+1) + "]"
			item_factor = robjects.r(item_factor)
			self.dialogUi.cb_variable_reco.addItem(str(item_factor[0]))

		self.dialogUi.cb_variable_reco.currentIndexChanged.connect(self.changeComboBox)

		rinterface.set_writeconsole(rinterface.consolePrint)

		# Signals
		QtCore.QObject.connect(self.dialogUi.buttonBox, QtCore.SIGNAL("accepted()"), self.accept, QtCore.Qt.UniqueConnection)
		QtCore.QObject.connect(self.dialogUi.buttonBox, QtCore.SIGNAL("rejected()"), self.cancel, QtCore.Qt.UniqueConnection)



	def changeComboBox(self):
		print "change combobox"
		self.variable = self.dialogUi.cb_variable_reco.currentText()
		nlevels = "length(levels(datos$" + self.variable + "))"
		nlevels = robjects.r(nlevels)
		nlevels = nlevels[0]

		self.dialogUi.nivel_table.clear()
		for k in range(self.dialogUi.nivel_table.rowCount()):
			self.dialogUi.nivel_table.removeRow(0)

		for i in range(nlevels):
			self.dialogUi.nivel_table.insertRow(self.dialogUi.nivel_table.rowCount())
			valor = "levels(datos$" + self.variable + ")[" + str(i+1) + "]"
			valor = robjects.r(valor)
			valor = valor[0]
			itemContent = QtGui.QTableWidgetItem(valor)
			self.dialogUi.nivel_table.setItem(i, 0, itemContent)



	def accept(self):
		print ("*Accept*")
		nombre_variable = self.dialogUi.le_nueva_variable.text()

		comando = "OBrienKaiser$" + nombre_variable + "=recode(OBrienKaiser$" + self.variable + ",\""

		for i in range(self.dialogUi.nivel_table.rowCount()):
			antiguo_valor = self.dialogUi.nivel_table.item(i, 0).text()
			nuevo_valor = self.dialogUi.nivel_table.item(i, 1).text()
			param = "'" + antiguo_valor + "'='" + nuevo_valor + "';"
			comando = comando + param

		comando = comando + "else=NA \")"

		self.ui.text_result.append("> " + comando)

		def f(x):
			self.ui.text_result.textCursor().insertText(x)

		rinterface.set_writeconsole(f)
		resultado = robjects.r(comando)
		self.ui.text_result.append(str(resultado))
		
		rinterface.set_writeconsole(rinterface.consolePrint)



	def cancel(self):
		print ("*Cancel*")


