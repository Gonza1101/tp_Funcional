// La Fila esta compuesta de
//      NumFila => numero de la fila y
//      CReina => la columna donde se encuentra la reina
case class Fila(NumFila:Int,CReina:Int)
type Tablero = List[Fila] // List(
                          //       Fila(0,0),
                          //       Fila(1,0) )
//----------------------------------- f() Base / f() AUX -----------------------------------------------------
def elModuloDe(n:Int):Int ={
  if(n>0){
    n
  }else{
    -n
  }
}
def crearTableroVacio(cantReina:Int,posicionInicial:Int):Tablero ={
  if (cantReina == posicionInicial){
    List()
  }else{
    Fila(posicionInicial,0)::crearTableroVacio(cantReina, posicionInicial+1)
  }
}
//------------------------------- F() sobre Tablero ---------------------------------------------------------
def modificarFila(fila:Fila,columnaReina:Int):Fila= Fila(fila.NumFila,columnaReina)

def modificarTablero(tablero: Tablero, filaACambiar:Int,columnaReina:Int):Tablero =
  tablero.updated(filaACambiar,modificarFila(tablero(filaACambiar),columnaReina) )

def ponerReinasEnTablero(cantReina:Int,filaInicial:Int,tableroActual:Tablero):List[Tablero]={
  if (cantReina == filaInicial){
    List(tableroActual)
  }else{
    (0 until cantReina).foldLeft(List.empty[Tablero]) ((acc,columna) =>
      acc ++ ponerReinasEnTablero(cantReina, filaInicial+1, modificarTablero(tableroActual,filaInicial,columna)))
  }
}

def nReinas(n:Int):List[Tablero]= ponerReinasEnTablero(n,0,crearTableroVacio(n,0))

//--------------------------------- f() de Verificacion --------------------------------------------------------
def hayReinasEnColumna(fila: Fila,columna:Int):Boolean = fila.CReina==columna

def cantReinaEnColumna(tablero:Tablero,columna:Int):Int= {
  tablero.indices.foldLeft(0)((acc,index) => if(hayReinasEnColumna(tablero(index),columna)) acc + 1 else acc)
}

def seComenHoV(tablero:Tablero):Boolean = {
  tablero.indices.foldLeft(false)((acc,index)=> if (cantReinaEnColumna(tablero,index) > 1) true else acc)
}

def hayReinaDiagonal(filaOrg:Fila,filaPrima:Fila): Boolean = elModuloDe(filaOrg.NumFila - filaPrima.NumFila) == elModuloDe(filaOrg.CReina - filaPrima.CReina )

def enDiagonal(tablero:Tablero,filaOrg:Int):Boolean = {
  tablero.indices.foldLeft(false)((acc, index)=> if ( index != filaOrg ) acc || hayReinaDiagonal(tablero(filaOrg),tablero(index))   else  acc)
}

def seComenDiagonal(tablero: Tablero): Boolean = {
  tablero.indices.foldLeft(false)((acc,index)=> if(enDiagonal(tablero,index)) true else acc)
}

def esTableroValido(tablero: Tablero)= !seComenHoV(tablero) && !seComenDiagonal(tablero)
//------------------------------  INICIAR JUEGO ------------------------------------------------------------------------

def solucionNReinas(n:Int):List[Tablero]= nReinas(n).filter(esTableroValido)

solucionNReinas(2)
solucionNReinas(3)
solucionNReinas(4)



