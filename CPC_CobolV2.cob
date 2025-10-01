       IDENTIFICATION DIVISION.
       PROGRAM-ID. CPC_COBOL.
       AUTHOR. "Andres Reyes Rojas".

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT DATOS-CLIENTES ASSIGN TO 'datosClientes.txt'
               ORGANIZATION IS LINE SEQUENTIAL.
           SELECT DATOS-CREDITOS ASSIGN TO 'datosCreditos.txt'
               ORGANIZATION IS LINE SEQUENTIAL.
           SELECT DATOS-TASA ASSIGN TO 'datosTasa.txt'
               ORGANIZATION IS LINE SEQUENTIAL.
           SELECT SALIDA ASSIGN TO 'resumenDelCredito.txt'
               ORGANIZATION IS LINE SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.
       FD DATOS-CLIENTES.
       01 REG-CLIENTE           PIC X(50).

       FD DATOS-CREDITOS.
       01 REG-CREDITO           PIC X(50).

       FD DATOS-TASA.
       01 REG-TASA              PIC X(50).

       FD SALIDA.
       01 REG-SALIDA            PIC X(200).

       WORKING-STORAGE SECTION.
       01 WS-EOF-SWITCHES.
          05 WS-EOF-CLIENTE       PIC X VALUE 'N'.
          05 WS-EOF-CREDITO       PIC X VALUE 'N'.
          05 WS-EOF-TASA          PIC X VALUE 'N'.
          05 WS-LINEA-ACTUAL      PIC 99 VALUE 0.

       01 WS-DATOS-CLIENTE.
          05 WS-NOMBRE            PIC X(20).
          05 WS-APELLIDO-PAT      PIC X(20).
          05 WS-APELLIDO-MAT      PIC X(20).
          05 WS-RUT               PIC X(12).
          05 WS-NUMERO-CREDITO    PIC X(12).

       01 WS-DATOS-CREDITO.
          05 WS-CRE-NUMERO        PIC X(12).
          05 WS-MONTO-SOLICITADO  PIC 9(12)V99.
          05 WS-FECHA-OTORGA      PIC X(10).
          05 WS-PERIODICIDAD      PIC 99.
          05 WS-CANT-CUOTAS       PIC 99.

       01 WS-DATOS-TASA.
          05 WS-TAS-NUMERO        PIC X(12).
          05 WS-TASA-ANUAL        PIC 99V99.
          05 WS-TASA-MENSUAL      PIC 9V99.

       01 WS-CAMPOS-FECHA.
          05 WS-DIA-OTORGA        PIC 99.
          05 WS-MES-OTORGA        PIC 99.
          05 WS-ANIO-OTORGA       PIC 9(4).
          05 WS-FECHA-ACTUAL.
             10 WS-DIA-ACT        PIC 99.
             10 WS-MES-ACT        PIC 99.
             10 WS-ANIO-ACT       PIC 9(4).

       01 WS-CALCULOS.
          05 WS-VALOR-CUOTA       PIC 9(12)V9(2).
          05 WS-SALDO-INICIAL     PIC 9(12)V9(2).
          05 WS-SALDO-FINAL       PIC 9(12)V9(2).
          05 WS-CAPITAL-AMORT     PIC 9(12)V9(2).
          05 WS-INTERES-CUOTA     PIC 9(12)V9(2).
          05 WS-TEMP1             PIC 9(12)V9(9).
          05 WS-TEMP2             PIC 9(12)V9(9).
          05 WS-TASA-MENSUAL-DEC  PIC 9V9(9).
          05 WS-TASA-ANUAL-DEC    PIC 9V9(9).
          05 WS-POTENCIA          PIC 9(12)V9(9).
          05 WS-CUOTA-ACTUAL      PIC 99.
          05 WS-FECHA-PAGO.
             10 WS-FP-DIA         PIC 99.
             10 WS-FP-MES         PIC 99.
             10 WS-FP-ANIO        PIC 9(4).
          05 WS-TOTAL-INTERESES   PIC 9(12)V9(2).
          05 WS-COSTO-TOTAL       PIC 9(12)V9(2).

       01 WS-DISPLAY-VARS.
          05 WS-DISPLAY-MONTO     PIC ZZ,ZZZ,ZZZ,ZZ9.99.
          05 WS-DISPLAY-SALDO     PIC ZZ,ZZZ,ZZZ,ZZ9.99.
          05 WS-DISPLAY-CUOTA     PIC ZZ,ZZZ,ZZZ,ZZ9.99.
          05 WS-DISPLAY-CAPITAL   PIC ZZ,ZZZ,ZZZ,ZZ9.99.
          05 WS-DISPLAY-INTERES   PIC ZZ,ZZZ,ZZZ,ZZ9.99.
          05 WS-DISPLAY-TOTAL-INT PIC ZZ,ZZZ,ZZZ,ZZ9.99.
          05 WS-DISPLAY-COSTO-TOT PIC ZZ,ZZZ,ZZZ,ZZ9.99.
          05 WS-FECHA-PAGO-FORM   PIC X(10).
          05 WS-FECHA-OTORGA-FORM PIC X(10).

       01 WS-LINEA-SALIDA.
          05 WS-LS-CUOTA          PIC Z9.
          05 FILLER               PIC X(3) VALUE " | ".
          05 WS-LS-FECHA          PIC X(10).
          05 FILLER               PIC X(3) VALUE " | ".
          05 WS-LS-SALDO-INICIAL  PIC ZZ,ZZZ,ZZZ,ZZ9.99.
          05 FILLER               PIC X(3) VALUE " | ".
          05 WS-LS-VALOR-CUOTA    PIC ZZ,ZZZ,ZZZ,ZZ9.99.
          05 FILLER               PIC X(3) VALUE " | ".
          05 WS-LS-CAPITAL        PIC ZZ,ZZZ,ZZZ,ZZ9.99.
          05 FILLER               PIC X(3) VALUE " | ".
          05 WS-LS-INTERES        PIC ZZ,ZZZ,ZZZ,ZZ9.99.
          05 FILLER               PIC X(3) VALUE " | ".
          05 WS-LS-SALDO-FINAL    PIC ZZ,ZZZ,ZZZ,ZZ9.99.

       01 WS-CABECERA-CLIENTE.
          05 FILLER               PIC X(5) VALUE "RUT: ".
          05 WS-CC-RUT            PIC X(12).
          05 FILLER               PIC X(5) VALUE " | ".
          05 FILLER               PIC X(16) VALUE "N° CREDITO: ".
          05 WS-CC-NUM-CREDITO    PIC X(12).

       01 WS-DIAS-POR-MES.
          05 WS-DIAS-MES OCCURS 12 TIMES 
                            INDEXED BY WS-I-MES.
             10 WS-DIAS           PIC 99.

       PROCEDURE DIVISION.
       MAIN-PROGRAM.
           PERFORM 1000-INICIALIZAR
           PERFORM 2000-PROCESAR-ARCHIVOS
           PERFORM 2500-PREPARAR-FECHAS
           PERFORM 3000-GENERAR-CUADRO-PAGO
           PERFORM 4000-FINALIZAR
           STOP RUN.

       1000-INICIALIZAR.
           OPEN INPUT DATOS-CLIENTES
           OPEN INPUT DATOS-CREDITOS
           OPEN INPUT DATOS-TASA
           OPEN OUTPUT SALIDA.
           
      *> Inicializar días por mes
           MOVE 31 TO WS-DIAS(1)   *> Enero
           MOVE 28 TO WS-DIAS(2)   *> Febrero
           MOVE 31 TO WS-DIAS(3)   *> Marzo
           MOVE 30 TO WS-DIAS(4)   *> Abril
           MOVE 31 TO WS-DIAS(5)   *> Mayo
           MOVE 30 TO WS-DIAS(6)   *> Junio
           MOVE 31 TO WS-DIAS(7)   *> Julio
           MOVE 31 TO WS-DIAS(8)   *> Agosto
           MOVE 30 TO WS-DIAS(9)   *> Septiembre
           MOVE 31 TO WS-DIAS(10)  *> Octubre
           MOVE 30 TO WS-DIAS(11)  *> Noviembre
           MOVE 31 TO WS-DIAS(12). *> Diciembre

       2000-PROCESAR-ARCHIVOS.
           PERFORM 2100-LEER-CLIENTES
           PERFORM 2200-LEER-CREDITOS
           PERFORM 2300-LEER-TASAS.

       2100-LEER-CLIENTES.
           MOVE 0 TO WS-LINEA-ACTUAL.
           PERFORM 5 TIMES
               READ DATOS-CLIENTES
                   AT END MOVE 'Y' TO WS-EOF-CLIENTE
               END-READ
               IF WS-EOF-CLIENTE = 'N'
                   ADD 1 TO WS-LINEA-ACTUAL
                   EVALUATE WS-LINEA-ACTUAL
                       WHEN 1 MOVE REG-CLIENTE TO WS-NOMBRE
                       WHEN 2 MOVE REG-CLIENTE TO WS-APELLIDO-PAT
                       WHEN 3 MOVE REG-CLIENTE TO WS-APELLIDO-MAT
                       WHEN 4 MOVE REG-CLIENTE TO WS-RUT
                       WHEN 5 MOVE REG-CLIENTE TO WS-NUMERO-CREDITO
                   END-EVALUATE
               END-IF
           END-PERFORM.

       2200-LEER-CREDITOS.
           MOVE 0 TO WS-LINEA-ACTUAL.
           PERFORM 5 TIMES
               READ DATOS-CREDITOS
                   AT END MOVE 'Y' TO WS-EOF-CREDITO
               END-READ
               IF WS-EOF-CREDITO = 'N'
                   ADD 1 TO WS-LINEA-ACTUAL
                   EVALUATE WS-LINEA-ACTUAL
                       WHEN 1 MOVE REG-CREDITO(1:12) TO WS-CRE-NUMERO
                       WHEN 2 MOVE FUNCTION NUMVAL(REG-CREDITO) 
                               TO WS-MONTO-SOLICITADO
                       WHEN 3 MOVE REG-CREDITO(1:10) TO WS-FECHA-OTORGA
                       WHEN 4 MOVE FUNCTION NUMVAL(REG-CREDITO) 
                               TO WS-PERIODICIDAD
                       WHEN 5 MOVE FUNCTION NUMVAL(REG-CREDITO) 
                               TO WS-CANT-CUOTAS
                   END-EVALUATE
               END-IF
           END-PERFORM.

       2300-LEER-TASAS.
           MOVE 0 TO WS-LINEA-ACTUAL.
           PERFORM 3 TIMES
               READ DATOS-TASA
                   AT END MOVE 'Y' TO WS-EOF-TASA
               END-READ
               IF WS-EOF-TASA = 'N'
                   ADD 1 TO WS-LINEA-ACTUAL
                   EVALUATE WS-LINEA-ACTUAL
                       WHEN 1 MOVE REG-TASA TO WS-TAS-NUMERO
                       WHEN 2 MOVE FUNCTION NUMVAL(REG-TASA) 
                               TO WS-TASA-ANUAL
                       WHEN 3 MOVE FUNCTION NUMVAL(REG-TASA) 
                               TO WS-TASA-MENSUAL
                   END-EVALUATE
               END-IF
           END-PERFORM.

       2500-PREPARAR-FECHAS.
      *> Convertir fecha de otorgamiento a campos numéricos
           MOVE WS-FECHA-OTORGA(1:2) TO WS-DIA-OTORGA
           MOVE WS-FECHA-OTORGA(4:2) TO WS-MES-OTORGA
           MOVE WS-FECHA-OTORGA(7:4) TO WS-ANIO-OTORGA
           
      *> Preparar fecha actual para primera cuota
           MOVE WS-DIA-OTORGA TO WS-DIA-ACT
           MOVE WS-MES-OTORGA TO WS-MES-ACT
           MOVE WS-ANIO-OTORGA TO WS-ANIO-ACT.

       3000-GENERAR-CUADRO-PAGO.
           PERFORM 3100-CALCULAR-VALOR-CUOTA
           PERFORM 3200-PREPARAR-CUADRO.

       3100-CALCULAR-VALOR-CUOTA.
           COMPUTE WS-TASA-MENSUAL-DEC = WS-TASA-MENSUAL / 100
           COMPUTE WS-TASA-ANUAL-DEC   = WS-TASA-ANUAL / 100

           COMPUTE WS-POTENCIA =
               (1 + WS-TASA-MENSUAL-DEC) ** WS-CANT-CUOTAS

           COMPUTE WS-TEMP1 = WS-MONTO-SOLICITADO *
                              (WS-TASA-MENSUAL-DEC * WS-POTENCIA)

           COMPUTE WS-TEMP2 = WS-POTENCIA - 1

           IF WS-TEMP2 NOT = 0
               COMPUTE WS-VALOR-CUOTA = WS-TEMP1 / WS-TEMP2
           ELSE
               MOVE WS-MONTO-SOLICITADO TO WS-VALOR-CUOTA
           END-IF.

       3200-PREPARAR-CUADRO.
           MOVE WS-MONTO-SOLICITADO TO WS-SALDO-INICIAL
           MOVE 0 TO WS-TOTAL-INTERESES

           DISPLAY "Iniciando el programa..."
           DISPLAY "TASA MENSUAL: " WS-TASA-MENSUAL-DEC
           DISPLAY "TASA ANUAL: " WS-TASA-ANUAL
           DISPLAY " "
           DISPLAY "VALOR DE LA CUOTA: " WS-VALOR-CUOTA
           DISPLAY "NOMBRE CLIENTE: " WS-NOMBRE
           DISPLAY " "

      *> Mostrar información del cliente como en la imagen
           MOVE WS-RUT TO WS-CC-RUT
           MOVE WS-NUMERO-CREDITO TO WS-CC-NUM-CREDITO
           DISPLAY WS-CABECERA-CLIENTE
           DISPLAY " "

      *> ESCRIBIR ENCABEZADOS EN ARCHIVO
           MOVE "CUADRO DE PAGOS" TO REG-SALIDA
           WRITE REG-SALIDA
           MOVE " " TO REG-SALIDA
           WRITE REG-SALIDA
           MOVE WS-CABECERA-CLIENTE TO REG-SALIDA
           WRITE REG-SALIDA
           MOVE " " TO REG-SALIDA
           WRITE REG-SALIDA
           MOVE "Cuota | Fecha       | Saldo         | Valor Cuota | Amort-Capital | Interes    | Saldo Final" 
             TO REG-SALIDA
           WRITE REG-SALIDA
           MOVE "------|-------------|---------------|-------------|---------------|------------|-------------" 
             TO REG-SALIDA
           WRITE REG-SALIDA

           DISPLAY "Cuota | Fecha       | Saldo         | Valor Cuota | Amort-Capital | Interes    | Saldo Final"
           DISPLAY "------|-------------|---------------|-------------|---------------|------------|-------------"

           *> MOSTRAR CUOTA 00 - VALOR ORIGINAL
           PERFORM 3230-MOSTRAR-CUOTA-00

           PERFORM VARYING WS-CUOTA-ACTUAL FROM 1 BY 1
                   UNTIL WS-CUOTA-ACTUAL > WS-CANT-CUOTAS
               PERFORM 3250-CALCULAR-FECHA-PAGO
               PERFORM 3210-CALCULAR-CUOTA
               PERFORM 3220-MOSTRAR-CUOTA
           END-PERFORM

           PERFORM 3240-MOSTRAR-RESUMEN.

       3210-CALCULAR-CUOTA.
           COMPUTE WS-INTERES-CUOTA =
               (WS-SALDO-INICIAL * WS-TASA-ANUAL-DEC * 30) / 360

           COMPUTE WS-CAPITAL-AMORT =
               WS-VALOR-CUOTA - WS-INTERES-CUOTA

           IF WS-CAPITAL-AMORT > WS-SALDO-INICIAL
               MOVE WS-SALDO-INICIAL TO WS-CAPITAL-AMORT
               COMPUTE WS-VALOR-CUOTA = WS-CAPITAL-AMORT +
                                        WS-INTERES-CUOTA
           END-IF

           COMPUTE WS-SALDO-FINAL =
               WS-SALDO-INICIAL - WS-CAPITAL-AMORT

           IF WS-SALDO-FINAL < 0
               MOVE 0 TO WS-SALDO-FINAL
           END-IF

           COMPUTE WS-TOTAL-INTERESES =
               WS-TOTAL-INTERESES + WS-INTERES-CUOTA.

       3220-MOSTRAR-CUOTA.
           MOVE WS-SALDO-INICIAL TO WS-DISPLAY-SALDO
           MOVE WS-VALOR-CUOTA   TO WS-DISPLAY-CUOTA
           MOVE WS-CAPITAL-AMORT TO WS-DISPLAY-CAPITAL
           MOVE WS-INTERES-CUOTA TO WS-DISPLAY-INTERES
           MOVE WS-SALDO-FINAL   TO WS-DISPLAY-SALDO

      *> Formatear fecha para display
           STRING WS-FP-DIA "-" WS-FP-MES "-" WS-FP-ANIO
             INTO WS-FECHA-PAGO-FORM
           END-STRING

      *> PREPARAR LINEA PARA ARCHIVO
           MOVE WS-CUOTA-ACTUAL TO WS-LS-CUOTA
           MOVE WS-FECHA-PAGO-FORM TO WS-LS-FECHA
           MOVE WS-SALDO-INICIAL TO WS-LS-SALDO-INICIAL
           MOVE WS-VALOR-CUOTA TO WS-LS-VALOR-CUOTA
           MOVE WS-CAPITAL-AMORT TO WS-LS-CAPITAL
           MOVE WS-INTERES-CUOTA TO WS-LS-INTERES
           MOVE WS-SALDO-FINAL TO WS-LS-SALDO-FINAL
           MOVE WS-LINEA-SALIDA TO REG-SALIDA
           WRITE REG-SALIDA

           DISPLAY WS-LS-CUOTA " | " 
                   WS-LS-FECHA " | "
                   WS-LS-SALDO-INICIAL " | "
                   WS-LS-VALOR-CUOTA " | "
                   WS-LS-CAPITAL " | "
                   WS-LS-INTERES " | "
                   WS-LS-SALDO-FINAL

           MOVE WS-SALDO-FINAL TO WS-SALDO-INICIAL.

       3230-MOSTRAR-CUOTA-00.
      *> Formatear fecha original
           STRING WS-DIA-OTORGA "-" WS-MES-OTORGA "-" WS-ANIO-OTORGA
             INTO WS-FECHA-OTORGA-FORM
           END-STRING

      *> ESCRIBIR CUOTA 00 EN ARCHIVO
           MOVE 0 TO WS-LS-CUOTA
           MOVE WS-FECHA-OTORGA-FORM TO WS-LS-FECHA
           MOVE WS-MONTO-SOLICITADO TO WS-LS-SALDO-INICIAL
           MOVE 0 TO WS-LS-VALOR-CUOTA
           MOVE 0 TO WS-LS-CAPITAL
           MOVE 0 TO WS-LS-INTERES
           MOVE WS-MONTO-SOLICITADO TO WS-LS-SALDO-FINAL
           MOVE WS-LINEA-SALIDA TO REG-SALIDA
           WRITE REG-SALIDA

           DISPLAY "00 | " 
                   WS-FECHA-OTORGA-FORM " | "
                   WS-LS-SALDO-INICIAL " | "
                   WS-LS-VALOR-CUOTA " | "
                   WS-LS-CAPITAL " | "
                   WS-LS-INTERES " | "
                   WS-LS-SALDO-FINAL.

       3240-MOSTRAR-RESUMEN.
           COMPUTE WS-COSTO-TOTAL =
               WS-MONTO-SOLICITADO + WS-TOTAL-INTERESES

           MOVE WS-TOTAL-INTERESES TO WS-DISPLAY-TOTAL-INT
           MOVE WS-COSTO-TOTAL TO WS-DISPLAY-COSTO-TOT

      *> ESCRIBIR RESUMEN EN ARCHIVO
           MOVE " " TO REG-SALIDA
           WRITE REG-SALIDA
           MOVE "RESUMEN CREDITO" TO REG-SALIDA
           WRITE REG-SALIDA
           MOVE " " TO REG-SALIDA
           WRITE REG-SALIDA

           STRING "Costo Total del Préstamo: $" 
                  WS-DISPLAY-COSTO-TOT
             INTO REG-SALIDA
           WRITE REG-SALIDA

           STRING "Importe Total de Intereses: $" 
                  WS-DISPLAY-TOTAL-INT
             INTO REG-SALIDA
           WRITE REG-SALIDA

           DISPLAY " "
           DISPLAY "================RESUMEN CREDITO================"
           DISPLAY "Costo Total del Préstamo: $" WS-DISPLAY-COSTO-TOT
           DISPLAY "Importe Total de Intereses: $" WS-DISPLAY-TOTAL-INT.

       3250-CALCULAR-FECHA-PAGO.
      *> Calcular fecha de pago sumando 30 días exactos
           COMPUTE WS-DIA-ACT = WS-DIA-ACT + 30
           
      *> Verificar si el día excede los días del mes actual
           SET WS-I-MES TO WS-MES-ACT
           IF WS-DIA-ACT > WS-DIAS(WS-I-MES)
      *> Ajustar al último día del mes si es febrero y año bisiesto
               IF WS-MES-ACT = 2 AND 
                  FUNCTION MOD(WS-ANIO-ACT, 4) = 0 AND
                  (FUNCTION MOD(WS-ANIO-ACT, 100) NOT = 0 OR
                   FUNCTION MOD(WS-ANIO-ACT, 400) = 0)
                   IF WS-DIA-ACT > 29
                       SUBTRACT 29 FROM WS-DIA-ACT
                       ADD 1 TO WS-MES-ACT
                   END-IF
               ELSE
                   SUBTRACT WS-DIAS(WS-I-MES) FROM WS-DIA-ACT
                   ADD 1 TO WS-MES-ACT
                   IF WS-MES-ACT > 12
                       SUBTRACT 12 FROM WS-MES-ACT
                       ADD 1 TO WS-ANIO-ACT
                   END-IF
               END-IF
           END-IF
           
      *> Mover a fecha de pago
           MOVE WS-DIA-ACT TO WS-FP-DIA
           MOVE WS-MES-ACT TO WS-FP-MES
           MOVE WS-ANIO-ACT TO WS-FP-ANIO.

       4000-FINALIZAR.
           CLOSE DATOS-CLIENTES
           CLOSE DATOS-CREDITOS
           CLOSE DATOS-TASA
           CLOSE SALIDA.
