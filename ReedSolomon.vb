Imports System.Text

Namespace RS

    ''' <summary>
    ''' Параметры кода Рида-Соломона, кодировщик/декодировщик.
    ''' </summary>
    ''' <remarks>
    ''' Author: aave, 2017.
    ''' Web: https://soltau.ru
    ''' </remarks>
    Partial Public Class ReedSolomon

#Region "CTORS"

        ''' <summary>
        ''' Initializes a new instance of the <see cref="ReedSolomon"/> class.
        ''' </summary>
        ''' <param name="codewordSymsNum">Число символов в кодовом слове, 2..N.</param>
        ''' <param name="dataSymsNum">Число значащих символов в сообщении, 1..N-1.</param>
        ''' <param name="bitsPerSymbol">Число битов в символе. Если не задано, то будет использовано максимально возможное число для данных параметров кода.</param>
        ''' <param name="fieldPoly">Многочлен поля (prime number). 
        ''' Для символов различной степени:
        ''' 3 -> x^3+x+1 = 11;
        ''' 4 -> x^4+x+1 = 19;
        ''' 5 -> x^5+x^2+1 = 37;
        ''' 6 -> x^6+x+1 = 67;
        ''' 7 -> x^7+x^3+1 = 137;
        ''' 8 -> x^8+x^4+x^3+x^2+1 = 285;
        ''' 9 -> x^9+x^4+1 = 529;
        ''' 10 -> x^10+x^3+1 = 1033;
        ''' 11 -> x^11+x^2+1 = 2053;
        ''' 12 -> x^12+x^6+x^4+x+1 = 4179.</param>
        ''' <param name="startGfGenFrom">С какого элемента поля Галуа начинать строить кодирующий многочлен: с 0-го, 1-го и тд.</param>
        Public Sub New(codewordSymsNum As Integer, dataSymsNum As Integer,
                       Optional bitsPerSymbol As Integer = -1, Optional fieldPoly As Integer = -1, Optional startGfGenFrom As Integer = 0)

            'Задаём число символов в кодовом слове и проверяем число битов в символе:
            If (1 < codewordSymsNum) Then
                _CodewordSymbolsNumber = codewordSymsNum
                Dim maxBitsNumberInSymbol As Integer = GetMaxBitsNumberInSymbol(CodewordSymbolsNumber)
                If (0 < bitsPerSymbol) AndAlso (bitsPerSymbol <= maxBitsNumberInSymbol) Then
                    _BitsInSymbol = bitsPerSymbol
                Else
                    _BitsInSymbol = maxBitsNumberInSymbol
                End If
            Else
                Throw New ArgumentException("Число символов в кодовом слове не может быть меньше 2.")
            End If

            'Задаём число символов данных:
            If (0 < dataSymsNum) AndAlso (dataSymsNum < CodewordSymbolsNumber) Then
                _DataSymbolsNumber = dataSymsNum
            Else
                _DataSymbolsNumber = GetMaxDataSymbolsNumber(CodewordSymbolsNumber, BitsInSymbol)
            End If

            'Задаём многочлен поля:
            If (0 < fieldPoly) Then
                _FieldPolynomial = fieldPoly
            Else
                Throw New ArgumentException("Многочлен поля задан неверно.")
            End If

            'Рассчитываем поле Галуа для данных параметров кода:
            Dim galoisFieldOrder As Integer = Me.CodewordSymbolsNumber
            If ((CodewordSymbolsNumber And 1) = 1) Then
                galoisFieldOrder += 1
            End If
            _GF = New GaloisField(FieldPolynomial, galoisFieldOrder)

            'Вычисляем кодирующий многочлен:
            _EncodingPolynomial = GF.GenerateEncodingPolynomial(FecSymbolsNumber, startGfGenFrom)
        End Sub

#End Region '/CTORS

#Region "READ-ONLY СВОЙСТВА"

        ''' <summary>
        ''' Конечное поле, в котором происходят все расчёты кода Рида-Соломона.
        ''' </summary>
        Public ReadOnly Property GF As GaloisField
            Get
                Return _GF
            End Get
        End Property
        Private _GF As GaloisField

        ''' <summary>
        ''' Простой (несократимый) многочлен для генерации поля Галуа.
        ''' </summary>
        ''' <remarks>Пример: P(x) = x^3 + x + 1 = 1011b = 11d.</remarks>
        Public ReadOnly Property FieldPolynomial As Integer
            Get
                Return _FieldPolynomial
            End Get
        End Property
        Private _FieldPolynomial As Integer = 285

        ''' <summary>
        ''' Число байтов в кодовом слове.
        ''' </summary>
        ''' <remarks>Max(CodewordBytesNumber) = 2^BitsInSymbol - 1.</remarks>
        Public ReadOnly Property CodewordSymbolsNumber As Integer
            Get
                Return _CodewordSymbolsNumber
            End Get
        End Property
        Private _CodewordSymbolsNumber As Integer = 255

        ''' <summary>
        ''' Число символов данных (длина сообщения).
        ''' </summary>
        Public ReadOnly Property DataSymbolsNumber As Integer
            Get
                Return _DataSymbolsNumber
            End Get
        End Property
        Private _DataSymbolsNumber As Integer = 239

        ''' <summary>
        ''' Число битов в одном символе Рида-Соломона.
        ''' </summary>
        ''' <remarks>n = 2^m - 1, где <see cref="CodewordSymbolsNumber">n</see> - число слов данных + FEC, <see cref="BitsInSymbol">m</see> - число битов в символе .</remarks>
        Public ReadOnly Property BitsInSymbol As Integer
            Get
                Return _BitsInSymbol
            End Get
        End Property
        Private _BitsInSymbol As Integer = 8

        ''' <summary>
        ''' Число проверочных FEC символов.
        ''' </summary>
        ''' <remarks>FEC == Forward Error Correction.</remarks>
        Public ReadOnly Property FecSymbolsNumber As Integer
            Get
                Dim fec As Integer = CodewordSymbolsNumber - DataSymbolsNumber
                Return fec
            End Get
        End Property

        ''' <summary>
        ''' Сколько ошибочных символов (errors) может восстановить данный код.
        ''' </summary>
        Public ReadOnly Property CanCorrectErrorsNumber As Integer
            Get
                If ((FecSymbolsNumber And 1) = 0) Then 'FEC - чётное
                    Return (FecSymbolsNumber \ 2)
                Else
                    Return (FecSymbolsNumber - 1) \ 2
                End If
            End Get
        End Property

        ''' <summary>
        ''' Сколько некорректных символов (erasures) может восстановить данный код.
        ''' </summary>
        Public ReadOnly Property CanCorrectErasuresNumber As Integer
            Get
                Return FecSymbolsNumber
            End Get
        End Property

        ''' <summary>
        ''' Корректирующая способность кода.
        ''' </summary>
        ''' <remarks>max error number = 2*e + v, где e - число ошибок (errors), v - число "опечаток" (erasures).</remarks>
        Public ReadOnly Property CorrectionPossibility As Integer
            Get
                Return (CanCorrectErrorsNumber + CanCorrectErrorsNumber + CanCorrectErasuresNumber)
            End Get
        End Property

        ''' <summary>
        ''' Максимальное значение кодового слова.
        ''' </summary>
        ''' <remarks>Если <see cref="CodewordSymbolsNumber ">CodewordSymbolsNumber</see> меньше <see cref="HighestCodeword">HighestCodeword</see>, 
        ''' это соответствует сокращённой форме кода.</remarks>
        Public ReadOnly Property HighestCodeword As Integer
            Get
                Return CInt((2 ^ BitsInSymbol) - 1)
            End Get
        End Property

        ''' <summary>
        ''' Коэффициенты кодирующего многочлена.
        ''' </summary>
        ''' <remarks>Например: 5x^2 + 3x + 2 = {5,3,2}.</remarks>
        ''' <value>G(x) = (x-a^1)(x-a^2)...(x-a^2t).</value>
        Public ReadOnly Property EncodingPolynomial As Integer()
            Get
                Return _EncodingPolynomial
            End Get
        End Property
        Private _EncodingPolynomial As Integer()

        ''' <summary>
        ''' Текстовое описание параметров Рида-Соломона.
        ''' </summary>
        Public ReadOnly Property CodeSpecification As String
            Get
                Dim sb As New StringBuilder()
                sb.AppendLine($"Спецификация RS({CodewordSymbolsNumber}, {DataSymbolsNumber}):")
                sb.AppendLine($"* битов в символе - [ {BitsInSymbol} ];")
                sb.AppendLine($"* проверочных FEC символов - [ {FecSymbolsNumber} ];")
                sb.AppendLine($"* восстановительная способность кода: ")
                sb.AppendLine($"  - ошибочных символов - [ {CanCorrectErrorsNumber} ];")
                sb.AppendLine($"  - ошибочных символов с известным положением - [ {CanCorrectErasuresNumber} ];")
                sb.AppendLine($"* кодирующий многочлен [ {GetPolynomialAsString(EncodingPolynomial)} ].")
                Return sb.ToString()
            End Get
        End Property

        ''' <summary>
        ''' Последнее декодированное сообщение.
        ''' </summary>
        Public Property Codeword As EncodedCodeword

#End Region '/READ-ONLY СВОЙСТВА

#Region "OPEN METHODS"

        ''' <summary>
        ''' Кодирует переданное сообщение с заданными параметрами кода Рида-Соломона.
        ''' </summary>
        ''' <param name="message">Сообщение, к которому будет добавлено заданное в конструкторе число FEC символов.</param>
        ''' <returns>Сообщение + FEC символы.</returns>
        Public Function Encode(ByVal message As Integer()) As Integer()
            If (message.Length > DataSymbolsNumber) Then
                Throw New ArgumentException($"Сообщение слишком длинное (максимальная длина сообщения {DataSymbolsNumber} символов).")
            Else
                Dim shiftedMessage As Integer() = GF.PolynomialShift(message, FecSymbolsNumber) 'сдвигаем многочлен на FEC символов.
                Dim divis As GaloisField.DivisionResult = GF.PolynomialDivision(shiftedMessage, EncodingPolynomial) 'остаток - это и есть код Рида-Соломона.
                Dim codeword As Integer() = GF.PolynomialAddition(shiftedMessage, divis.Remainder)
                Return codeword
            End If
        End Function

        ''' <summary>
        ''' Восстановливает исходное сообщение и исходные FEC символы. 
        ''' Если в сообщении обнаруживаются ошибки/пропуски символов, пытается скорректировать их.
        ''' </summary>
        ''' <param name="encodedMessage">Сообщение + FEC символы, которое нужно декодировать.</param>
        ''' <returns>Восстановленное исходное сообщение с корректировкой ошибок. FEC сохраняется в свойстве CorrectedFec().</returns>
        Public Function Decode(ByVal encodedMessage As Integer()) As Integer()
            If (encodedMessage.Length > CodewordSymbolsNumber) Then
                Throw New ArgumentException($"Сообщение слишком длинное (максимальная длина сообщения {CodewordSymbolsNumber} символов).")
            Else
                Codeword = New EncodedCodeword(GF, encodedMessage, FecSymbolsNumber)
                Return Codeword.CorrectedMessage
            End If
        End Function

        ''' <summary>
        ''' Восстановливает исходное сообщение и исходные FEC символы, если известны позиции ошибок (erasures). 
        ''' Если в сообщении обнаруживаются ошибки/пропуски символов, пытается скорректировать их.
        ''' </summary>
        ''' <param name="encodedMessage">Сообщение, которое нужно декодировать + FEC символы.</param>
        ''' <param name="erasurePositions">Массив индексов ошибочных/пропущенных символов, начиная с 0. Здесь могут быть индексы не всех ошибок, а лишь некоторых.</param>
        ''' <returns>Восстановленное исходное сообщение с корректировкой ошибок.</returns>
        Public Function Decode(ByVal encodedMessage As Integer(), ByVal erasurePositions As Integer()) As Integer()
            If (encodedMessage.Length > CodewordSymbolsNumber) Then
                Throw New ArgumentException($"Сообщение слишком длинное (максимальная длина сообщения {CodewordSymbolsNumber} символов).")
            Else
                Codeword = New EncodedCodeword(GF, encodedMessage, FecSymbolsNumber, erasurePositions)
                Return Codeword.CorrectedMessage
            End If
        End Function

#End Region '/OPEN METHODS

#Region "ЗАКРЫТЫЕ МЕТОДЫ"

        ''' <summary>
        ''' Возвращает наибольшее возможное значение числа символов данных.
        ''' </summary>
        ''' <param name="codewordSymbolsNumber">Число символов в кодовом слове.</param>
        ''' <param name="bitsInSymbol">Число битов в символе.</param>
        Private Function GetMaxDataSymbolsNumber(ByVal codewordSymbolsNumber As Integer, ByVal bitsInSymbol As Integer) As Integer
            Dim res As Integer = codewordSymbolsNumber - bitsInSymbol
            Return res
        End Function

        ''' <summary>
        ''' Возвращает максимальное число элементов, возможное при данном числе символов в кодовом слове.
        ''' </summary>
        ''' <param name="codewordSymbolsNumber">Число символов в кодовом слове.</param>
        Private Function GetMaxBitsNumberInSymbol(ByVal codewordSymbolsNumber As Integer) As Integer
            Dim n As Integer = codewordSymbolsNumber
            If (codewordSymbolsNumber Mod 2 <> 0) Then
                n += 1
            End If
            Dim maxBitsNumberInSymbol As Integer = CInt(Math.Log(n, 2))
            Return maxBitsNumberInSymbol
        End Function

#End Region '/ЗАКРЫТЫЕ МЕТОДЫ

#Region "ВСПОМОГАТЕЛЬНЫЕ"

        ''' <summary>
        ''' Возвращает многочлен в виде строки вида P(X) = AX^3 + BX^2 + CX + D.
        ''' </summary>
        ''' <param name="poly">Коэффициенты многочлена. Например: {5, 4, 3} == 5x^2 + 4x^1 + 3x^0 == 5x^2 + 4x + 3.</param>
        ''' <param name="reversed">Обращённый ли порядок членов (наименее значимый - с индексом "0" в массиве).
        ''' Например для многочлена {5, 3, 1}: False - 5x^2 + 3x + 1, True - 1x^2 + 3x + 5.</param>
        ''' <param name="showPolyView">Отображать ли многочлен в виде коэффициентов при X: 5x^2 + 3x.</param>
        ''' <param name="showArrayView">Отображать ли многочлен в виде массива коэффициентов: {5,3,0}.</param>
        ''' <param name="showPx">Показывать название многочлена.</param>
        Public Shared Function GetPolynomialAsString(ByVal poly As Integer(),
                                                     Optional ByVal reversed As Boolean = False,
                                                     Optional showPolyView As Boolean = True,
                                                     Optional showArrayView As Boolean = True,
                                                     Optional showPx As String = "P(x)") As String
            Dim sb As New StringBuilder(showPx)
            Dim sbPoly As New StringBuilder
            Dim sbArr As New StringBuilder

            For i As Integer = poly.Length - 1 To 0 Step -1
                Dim ind As Integer = i
                If (Not reversed) Then
                    ind = poly.Length - 1 - i
                End If

                'В виде полинома:
                If showPolyView Then
                    If (poly(ind) > 0) Then 'скрывает элементы с нулевыми коэффициентами 
                        sbPoly.Append(" + ")
                        If (poly(ind) > 1) Then 'не отображаем коэффициент, равный "1"
                            sbPoly.Append($"{poly(ind)}")
                        End If
                        sbPoly.Append($"x^{i}") 'отображаем степень
                    End If
                End If

                'В виде массива:
                If showArrayView Then
                    If (ind > 0) Then
                        sbArr.Append(",")
                    End If
                    If (poly(ind) > 0) Then 'скрывает элементы с нулевыми коэффициентами                
                        If (poly(ind) > 1) Then 'не отображаем коэффициент, равный "1"
                            sbArr.Append($"{poly(ind)}")
                        ElseIf (poly(ind) = 1) Then
                            sbArr.Append("1")
                        End If
                    Else
                        sbArr.Append("0")
                    End If
                    If (ind > 0) Then
                        sbArr.Append(",")
                    End If
                End If
            Next

            If showPolyView Then
                sbPoly.Insert(0, " = ")
                sb.Append(sbPoly)
            End If

            If showArrayView Then
                sbArr.Insert(0, " = {")
                sbArr.Append("}")
                sb.Append(sbArr)
            End If

            sb.Replace(",,", ",")
            sb.Replace(",}", "}")
            sb.Replace("  ", " ")
            sb.Replace("= +", "= ")

            Return sb.ToString()

        End Function

        ''' <summary>
        ''' Возвращает строку, представляющую число в виде многочлена c коэффициентами "1".
        ''' </summary>
        ''' <param name="int">Число, биты которого в бинарном представлении соответствуют позициям ненулевых коэффициентов в многочлене. 
        ''' Например: b'1011 = 1*x^3 + 0*x^2 + 1*x^1 + 1*x^0 = x^3 + x + 1.</param>
        Public Shared Function GetIntAsPolynomial(ByVal int As Integer) As String
            Dim ba As New BitArray({int})
            Dim sb As New StringBuilder
            For i As Integer = ba.Length - 1 To 0 Step -1
                If ba(i) Then
                    sb.Append($"x^{i} + ")
                End If
            Next
            sb.Remove(sb.Length - 3, 3)
            Return sb.ToString()
        End Function

#End Region '/ВСПОМОГАТЕЛЬНЫЕ

#Region "NESTED TYPES"

#End Region '/NESTED TYPES

    End Class '/ReedSolomon

End Namespace 'RS