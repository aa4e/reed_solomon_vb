Imports System.Text

Namespace RS

    ''' <summary>
    ''' Арифметика в полях Галуа, в том числе действия над многочленами.
    ''' </summary>
    Public Class GaloisField

#Region "CTOR"

        ''' <summary>
        ''' Инициализирует экземпляр класса <see cref="GaloisField"/>.
        ''' </summary>
        ''' <param name="prime">Неделимый многочлен (эквивалент простого числа, prime number) степени из диапазона [2^m...2^(m+1)], m = log(2, degree).
        ''' Для символов различной длины:
        ''' <list type="bullet">
        ''' <item>3-11;</item> 
        ''' <item>4-19;</item> 
        ''' <item>5-37;</item>  
        ''' <item>6-67;</item> 
        ''' <item>7-137;</item> 
        ''' <item>8-285;</item> 
        ''' <item>9-529;</item> 
        ''' <item>10-1033;</item> 
        ''' <item>11-2053;</item> 
        ''' <item>12-4179.</item> 
        ''' </list>
        ''' </param>
        ''' <param name="size">Размер поля Галуа (число элементов поля). Значение должно быть степенью двойки: GF(size) = GF(2^degree).</param>    
        ''' <exception cref="ArgumentException">
        ''' Многочлен должен быть положительным простым числом.
        ''' или
        ''' Размер поля Галуа должен быть 2 элемента и более.
        ''' </exception>
        Public Sub New(ByVal prime As Integer, ByVal size As Integer)
            If (prime < 1) Then
                Throw New ArgumentException("Многочлен поля должен быть положительным простым числом.")
            End If
            If (size < 2) Then
                Throw New ArgumentException("Размер поля Галуа должен быть не менее 2-х элементов.")
            End If
            _FieldPolynomial = prime
            _FieldSize = size
            GenerateLookupTable()
        End Sub

#End Region '/CTOR

#Region "READ-ONLY PROPS"

        ''' <summary>
        ''' Неделимый многочлен (эквивалент простого числа), используемый для вычисления образующего многочлена (синоним: prime number).
        ''' </summary>
        ''' <value>Эквивалент простого числа, значение которого лежит в диапазоне [ 2^Degree...2^(Degree+1) ].</value>
        ''' <remarks>
        ''' Пример: P(x) = x^3 + x^1 + x^0 = 1011b = 11D.
        ''' </remarks>
        Public ReadOnly Property FieldPolynomial As Integer
            Get
                Return _FieldPolynomial
            End Get
        End Property
        Private _FieldPolynomial As Integer = 285

        ''' <summary>
        ''' Число элементов в поле Галуа (характеристика поля, field characteristic, field size).
        ''' </summary>
        ''' <value>Значение должно быть степенью двойки: GF(FieldSize) = GF(2^m).</value>
        Public ReadOnly Property FieldSize As Integer
            Get
                Return _FieldSize
            End Get
        End Property
        Private _FieldSize As Integer = 256

        ''' <summary>
        ''' Степень поля Галуа.
        ''' </summary>
        Public ReadOnly Property Degree As Integer
            Get
                Dim deg As Double = Math.Log(FieldSize, 2)
                Return CInt(deg)
            End Get
        End Property

        ''' <summary>
        ''' Наибольшее значение из всех элементов поля Галуа.
        ''' </summary>
        Public ReadOnly Property MaxFieldValue As Integer
            Get
                Return (FieldSize - 1)
            End Get
        End Property

#End Region '/READ-ONLY PROPS

#Region "МЕТОДЫ ДОСТУПА К ТАБЛИЦАМ LUT С ПРОВЕРКОЙ ГРАНИЦ"

        ''' <summary>
        ''' Возвращает элемент поля по его индексу.
        ''' </summary>
        ''' <param name="index">Индекс элемента.</param>
        Public Function ElementAt(ByVal index As Integer) As Integer
            'Dim l As Integer = GF.MaxFieldValue - coef_pos(i)
            'Xj.Add(GF.GaloisPow(2, -l))
            Dim elem As Integer = Exponents(index)
            Return elem
        End Function

        ''' <summary>
        ''' Элементы поля Галуа.
        ''' </summary>
        ''' <remarks>Значения элементов соответствуют позициям битов в двоичном представлении или позиции элементов "X" многочлена.</remarks>
        Public Function Exponents() As Integer()
            Return _Exponents
        End Function
        Private _Exponents As Integer()

        ''' <summary>
        ''' Элементы поля Галуа (позиции битов в двоичном представлении или позиции ненулевых элементов многочлена).
        ''' </summary>
        ''' <param name="index">Индекс элемента.</param>
        ''' <remarks>Значения элементов соответствуют позициям битов в двоичном представлении или позиции элементов "X" многочлена.</remarks>
        Public Function Exponents(ByVal index As Integer) As Integer
            If (index < 0) Then
                Debug.Assert(index >= 0, "index >= 0 !")
            End If
            If (index >= MaxFieldValue) Then
                index = index Mod MaxFieldValue
            End If
            Return _Exponents(index)
        End Function

        ''' <summary>
        ''' Степени коэффициентов альфа данного конечного поля.
        ''' </summary>
        Public Function Logarithms() As Integer()
            Return _Logarithms
        End Function
        Private _Logarithms As Integer()

        ''' <summary>
        ''' Степени коэффициентов альфа данного конечного поля.
        ''' </summary>
        ''' <param name="index">Индекс элемента. Запрещены нуль и кратные MaxFieldValue значения.</param>
        ''' <exception cref="ArgumentException">Логарифм не может равняться нулю.</exception>
        Public Function Logarithms(ByVal index As Integer) As Integer
            If (index < 0) Then
                Debug.Assert(index >= 0, "index >= 0 !")
            ElseIf (index Mod FieldSize = 0) Then
                Debug.Assert(index Mod FieldSize <> 0, "Логарифм не может равняться нулю.")
                '    Throw New ArgumentException("Логарифм не может равняться нулю.")
            End If
            If (index > MaxFieldValue) Then
                index = index And MaxFieldValue
            End If
            Return _Logarithms(index)
        End Function

#End Region '/МЕТОДЫ ДОСТУПА К ТАБЛИЦАМ LUT С ПРОВЕРКОЙ ГРАНИЦ

#Region "GF ARITHMETIC WITH LUT"

        ''' <summary>
        ''' Складывает/вычитает два числа в конечном поле.
        ''' </summary>
        ''' <param name="x">Первое слагаемое.</param>
        ''' <param name="y">Второе слагаемое.</param>
        ''' <remarks>Т.к. используется двоичная арифметика без сдвига, сложение и вычитание в конечных полях эквивалентны.</remarks>
        Public Function GaloisAddSubtract(ByVal x As Integer, ByVal y As Integer) As Integer
            Return (x Xor y)
        End Function

        ''' <summary>
        ''' Умножает два числа в конечном поле, используя предварительно рассчитанную таблицу (Lookup table).
        ''' </summary>
        ''' <param name="x">Первый множитель.</param>
        ''' <param name="y">Второй множитель.</param>
        Public Function GaloisMultiply(ByVal x As Integer, ByVal y As Integer) As Integer
            If (x = 0) OrElse (y = 0) Then
                Return 0
            Else
                Dim res As Integer = Exponents(Logarithms(x) + Logarithms(y))
                Return res
            End If
        End Function

        ''' <summary>
        ''' Делит два числа в конечном поле.
        ''' </summary>
        ''' <param name="dividend">Делимое.</param>
        ''' <param name="divisor">Делитель.</param>
        Public Function GaloisDivide(ByVal dividend As Integer, ByVal divisor As Integer) As Integer
            If (divisor = 0) Then
                Throw New DivideByZeroException("Делитель не может равняться нулю.")
            ElseIf (dividend = 0) Then
                Return 0
            Else
                'Dim multInverse As Integer = GetMultiplicativeInverse(divisor)
                'Dim res As Integer = GaloisMultiply(dividend, multInverse)
                Dim logSubs As Integer = Logarithms(dividend) - Logarithms(divisor)
                If (logSubs < 0) Then
                    logSubs += MaxFieldValue
                End If
                Dim res As Integer = Exponents(logSubs)
                Return res
            End If
        End Function

        ''' <summary>
        ''' Возводит число в заданную степень в конечном поле.
        ''' </summary>
        ''' <param name="x">Основание степени.</param>
        ''' <param name="power">Показатель степени. Допускаются отрицательные степени.</param>
        Public Function GaloisPow(ByVal x As Integer, ByVal power As Integer) As Integer
            Dim res As Integer
            If (power >= 0) Then
                res = Exponents(Logarithms(x) * power)
            Else
                res = GaloisDivide(1, Exponents(Logarithms(x) * -power))
            End If
            Return res
        End Function

        ''' <summary>
        ''' Умножает два числа в конечном поле без использования предварительно рассчитанной таблицы (Lookup table, LUT).
        ''' </summary>
        ''' <param name="x">Первый множитель.</param>
        ''' <param name="y">Второй множитель.</param>
        Public Function GaloisMultiplyNoLUT(ByVal x As Integer, ByVal y As Integer) As Integer
            Dim result As Integer = CarrylessMult(x, y)
            If (FieldPolynomial > 0) Then
                result = CarrylessDivision(result, FieldPolynomial) 'сокращение результата (modular reduction).
            End If
            Return result
        End Function

        ''' <summary>
        ''' Умножает два числа в конечном поле без LUT по алгоритму Russian Peasant Algorithm.
        ''' Galois Field integer multiplication using Russian Peasant Multiplication algorithm (faster than the standard multiplication + modular reduction).
        ''' </summary>
        ''' <param name="x">Первый множитель.</param>
        ''' <param name="y">Второй множитель.</param>
        ''' <param name="carryless">Используется арифметика без переноса.</param>
        ''' <remarks>Если prime=0 (нет сокращения полинома по модулю) и carryless=false (не используется арифметика без переноса), это будет эквивалент обычного умножения.</remarks>
        Public Function GaloisMultiplyNoLUTPeasant(ByVal x As Integer, ByVal y As Integer, Optional carryless As Boolean = True) As Integer
            Dim result As Integer = 0
            Do While (y > 0)
                If ((y And 1) = 1) Then 'для нечётных Y:
                    result = result Xor x
                    If (Not carryless) Then
                        result += x 'add the corresponding x to r (the sum of all x's corresponding to odd y's will give the final product). 
                        'Since we're in GF(2), the addition is in fact an XOR (very important because in GF(2) the multiplication and additions are carry‐less, thus it changes the result!).
                    End If
                End If
                y >>= 1 'equivalent to y/2
                x <<= 1 'equivalent to x*2
                If (FieldPolynomial > 0) AndAlso ((x And FieldSize) = FieldSize) Then
                    x = x Xor FieldPolynomial 'Если x>=256, применяем сокращение по модулю, используя простой многочлен поля.
                    '(на самом деле вычитаем, но т.к. простой полином поля может быть >=256, то используем XOR).
                End If
            Loop
            Return result
        End Function

#End Region 'GF ARITHMETIC WITH LUT

#Region "POLYNOMIAL ARITHMETIC"

        ''' <summary>
        ''' Вычисляет многочлен в заданной точке в поле Галуа.
        ''' </summary>
        ''' <param name="poly">Коэффициенты многочлена, начиная со старшей степени: x^2 + 2x + 3 = {1,2,3}.</param>
        ''' <param name="x">Точка, в которой нужно вычислить значение многочлена.</param>
        Public Function PolynomialEval(ByVal poly As Integer(), ByVal x As Integer) As Integer
            Dim y As Integer = poly(0)
            For i As Integer = 1 To poly.Length - 1
                y = GaloisMultiply(y, x) Xor poly(i)
            Next
            Return y
        End Function

        ''' <summary>
        ''' Сдвигает многочлен на заданное количество разрядов.
        ''' </summary>
        ''' <param name="poly">Коэффициенты многочлена, начиная со старшей степени: x^2 + 2x + 3 = {1,2,3}.</param>
        ''' <param name="n">Количество разрядов, на которое сдвигается многочлен.</param>
        ''' <remarks>Эквивалентно умножению многочлена на X^N.</remarks>
        Public Function PolynomialShift(ByVal poly As Integer(), ByVal n As Integer) As Integer()
            Dim shiftedPoly As Integer() = poly
            ReDim Preserve shiftedPoly(poly.Length + n - 1)
            Return shiftedPoly
        End Function

        ''' <summary>
        ''' Сдвигает многочлен на заданное количество разрядов.
        ''' </summary>
        ''' <param name="poly">Коэффициенты многочлена, начиная со старшей степени: x^2 + 2x + 3 = {1,2,3}.</param>
        ''' <param name="n">Количество разрядов, на которое сдвигается многочлен.</param>
        ''' <remarks>Эквивалентно умножению многочлена на X^N.</remarks>
        Public Function PolynomialShiftSlower(ByVal poly As Integer(), ByVal n As Integer) As Integer()
            Dim placeholder As New List(Of Integer)({1})
            placeholder.AddRange(Enumerable.Repeat(0, n))
            Dim shiftedPoly As Integer() = PolynomialMultiplication(poly, placeholder.ToArray())
            Return shiftedPoly
        End Function

        ''' <summary>
        ''' Суммирует два многочлена в конечном поле.
        ''' </summary>
        ''' <param name="p">Первое слагаемое - коэффициенты многочлена в виде: 5x^2 + 3x + 1 = {5,3,1}.</param>
        ''' <param name="q">Второе слагаемое - коэффициенты многочлена в виде: 5x^2 + 3x + 1 = {5,3,1}.</param>
        ''' <remarks>Например: (5x^3+3x^2+7x+16) + (2x^2+3x+4) = 5x^3+x^2+4x+20.</remarks>
        Public Function PolynomialAddition(ByVal p() As Integer, ByVal q() As Integer) As Integer()
            Dim r(({p.Length, q.Length}).Max - 1) As Integer
            For i As Integer = 0 To p.Length - 1
                r(i + r.Length - p.Length) = p(i)
            Next
            For j As Integer = 0 To q.Length - 1
                r(j + r.Length - q.Length) = r(j + r.Length - q.Length) Xor q(j)
            Next
            Return r
        End Function

        ''' <summary>
        ''' Умножает многочлен на скаляр.
        ''' </summary>
        ''' <param name="p">Коэффициенты многочлена, начиная со старшей степени: x^2 + 2x + 3 = {1,2,3}.</param>
        ''' <param name="x">Скаляр.</param>
        Public Function PolynomialMultiplicationScalar(ByVal p() As Integer, ByVal x As Integer) As Integer()
            Dim r(p.Length - 1) As Integer
            For i As Integer = 0 To p.Length - 1
                r(i) = GaloisMultiply(p(i), x)
            Next
            Return r
        End Function

        ''' <summary>
        ''' Вычисляет произведение двух многочленов в конечном поле.
        ''' </summary>
        ''' <param name="p">Первый множитель - коэффициенты многочлена, начиная со старшей степени: x^2 + 2x + 3 = {1,2,3}).</param>
        ''' <param name="q">Второй множитель - коэффициенты многочлена, начиная со старшей степени: x^2 + 2x + 3 = {1,2,3}).</param>
        Public Function PolynomialMultiplication(ByVal p() As Integer, ByVal q() As Integer) As Integer()
            Dim result(p.Length + q.Length - 2) As Integer 'NOTE Почему при -1 (вместо -2) размер массива становится больше на 1?!
            For j As Integer = 0 To q.Length - 1
                For i As Integer = 0 To p.Length - 1
                    result(i + j) = GaloisAddSubtract(result(i + j), GaloisMultiply(p(i), q(j)))
                Next
            Next
            Return result
        End Function

        ''' <summary>
        ''' Вычисляет произведение двух многочленов в конечном поле без использования предварительно рассчитанных таблиц.
        ''' </summary>
        ''' <param name="p">Первый множитель - коэффициенты многочлена, начиная со старшей степени: x^2 + 2x + 3 = {1,2,3}).</param>
        ''' <param name="q">Второй множитель - коэффициенты многочлена, начиная со старшей степени: x^2 + 2x + 3 = {1,2,3}).</param>
        Public Function PolynomialMultiplicationNoLUT(ByVal p() As Integer, ByVal q() As Integer) As Integer()
            Dim result(p.Length + q.Length - 2) As Integer 'NOTE Почему при "-1" (вместо "-2") размер массива становится больше на 1?!
            For j As Integer = 0 To q.Length - 1
                For i As Integer = 0 To p.Length - 1
                    result(i + j) = GaloisAddSubtract(result(i + j), GaloisMultiplyNoLUT(p(i), q(j)))
                Next
            Next
            Return result
        End Function

        ''' <summary>
        ''' Делит два многочлена в конечном поле и возвращает делимое и остаток от деления.
        ''' </summary>
        ''' <param name="dividend">Коэффициенты многочлена делимого, например: 2*x^2 + 4*x + 3 = {2,4,3}. Порядок делимого должен быть не меньше порядка делителя.</param>
        ''' <param name="divisor">Коэффициенты многочлена делителя, например: x^2 + 2*x + 3 = {1,2,3}.</param>
        Public Function PolynomialDivision(ByVal dividend As Integer(), ByVal divisor As Integer()) As DivisionResult
            If (dividend.Length < divisor.Length) Then
                Throw New ArithmeticException("Порядок многочлена делимого должен быть не меньше порядка многочлена делителя.")
            End If
            If (dividend(0) = 0) Then
                'Throw New ArithmeticException("Старший член многочлена делимого не может быть равен 0.")
            End If
            If (divisor(0) = 0) Then
                Throw New ArithmeticException("Старший член многочлена делителя не может быть равен 0.")
            End If

            Dim remainder As New List(Of Integer)(dividend)
            Dim quotient As List(Of Integer) = Enumerable.Repeat(0, remainder.Count - divisor.Length + 1).ToList()

            Dim divLen As Integer = divisor.Length - 1
            Dim remLen As Integer = remainder.Count - 1
            Dim quotLen As Integer = quotient.Count - 1

            For i As Integer = quotLen To 0 Step -1
                quotient(quotLen - i) = GaloisDivide(remainder(quotLen - i), divisor(0))
                For j As Integer = divLen To 0 Step -1
                    Dim m As Integer = GaloisMultiply(quotient(quotLen - i), divisor(divLen - j))
                    remainder(remLen - i - j) = GaloisAddSubtract(remainder(remLen - i - j), m)
                Next
            Next

            Dim divRes As DivisionResult
            With divRes
                'Убираем незначащие нули в начале остатка:
                'TODO В некоторых случаях они нужны (например, при отзеркаливании полинома). Как определить, в каких?
                Do While (remainder.Count > 1) AndAlso (remainder(0) = 0)
                    remainder.RemoveAt(0)
                Loop
                .Remainder = remainder.ToArray()
                .Quotient = quotient.ToArray()
            End With
            Return divRes
        End Function

        ''' <summary>
        ''' Результат деления: частное и остаток.
        ''' </summary>
        Public Structure DivisionResult
            ''' <summary>
            ''' Частное.
            ''' </summary>
            Public Quotient As Integer()
            ''' <summary>
            ''' Остаток от деления.
            ''' </summary>
            Public Remainder As Integer()
        End Structure

#End Region '/POLYNOMIAL ARITHMETIC

#Region "ВСПОМОГАТЕЛЬНЫЕ"

        ''' <summary>
        ''' Возвращает аддитивную инверсию числа в поле Галуа.
        ''' </summary>
        ''' <param name="number">Элемент поля Галуа, для которого нужно найти аддитивную инверсию.</param>
        ''' <remarks>Аддитивная инверсия: a + (-a) = 0.</remarks>
        Public Function GetAdditiveInverse(ByVal number As Integer) As Integer
            Dim res As Integer = MaxFieldValue - number
            Return res
        End Function

        ''' <summary>
        ''' Возвращает мультипликативную инверсию числа в поле Галуа.
        ''' </summary>
        ''' <param name="number">Элемент поля Галуа, для которого нужно найти мультипликативную инверсию.</param>
        ''' <remarks>Мультипликативная инверсия: a*(a^-1) = 1б т.е. Inverse(x) == GaloisDivide(1, x).</remarks>
        Public Function GetMultiplicativeInverse(ByVal number As Integer) As Integer
            If (number = 0) Then
                Debug.Assert(False, "Мультипликативная инверсия для числа нуль не должна быть использована.")
                Return 0
            Else
                Dim invertedNumber As Integer = Exponents(MaxFieldValue - Logarithms(number))
                Return invertedNumber
            End If
        End Function

        ''' <summary>
        ''' Выводит параметры поля Галуа (многочлен поля и элементы поля).
        ''' </summary>
        Public Overrides Function ToString() As String
            Dim sb As New StringBuilder("Образующий многочлен поля:" & ControlChars.NewLine)
            sb.AppendLine($"P(x) = 0x{Convert.ToString(FieldPolynomial, 16).ToUpper()} = b'{Convert.ToString(FieldPolynomial, 2)} = {ReedSolomon.GetIntAsPolynomial(FieldPolynomial)}")
            sb.AppendLine()
            sb.AppendLine($"Элементы поля GF({FieldSize}):")
            Dim cnt As Integer = 0
            For i As Integer = 0 To FieldSize - 1
                cnt += 1
                sb.Append(Convert.ToString(Exponents(i), 16))
                sb.Append(" ")
                If (cnt = 8) Then
                    sb.AppendLine()
                    cnt = 0
                End If
            Next
            Return sb.ToString()
        End Function

#End Region '/ВСПОМОГАТЕЛЬНЫЕ

#Region "CLOSED METHODS"

        ''' <summary>
        ''' Рассчитывает таблицы экспонент и логарифмов элементов конечного поля.
        ''' </summary>
        Private Sub GenerateLookupTable()
            _Exponents = New Integer(FieldSize - 1) {}
            _Logarithms = New Integer(FieldSize - 1) {}
            Dim x As Integer = 1
            For i As Integer = 0 To MaxFieldValue - 1
                _Exponents(i) = x
                _Logarithms(x) = i
                x <<= 1 'в конечном поле с образующим многочленом степени 2 это эквивалентно: x = GaloisMultiplyNoLUT(x, 2), но выполняется быстрее.
                If (x >= FieldSize) Then
                    x = x Xor FieldPolynomial
                    x = x And MaxFieldValue
                End If
            Next
        End Sub

        <Obsolete()>
        Private Sub GenerateLookupTable1()
            _Exponents = New Integer(FieldSize + FieldSize - 1) {}
            _Logarithms = New Integer(FieldSize - 1) {}

            Dim x As Integer = 1
            For i As Integer = 0 To 254
                _Exponents(i) = x
                _Logarithms(x) = i
                x = GaloisMultiplyNoLUT(x, 2)
            Next
            For i As Integer = 255 To 511
                _Exponents(i) = _Exponents(i - 255)
            Next
        End Sub

        ''' <summary>
        ''' Генерирует кодирующий многочлен.
        ''' </summary>
        ''' <param name="fecSymbolsNumber">Число FEC-символов.</param>
        ''' <param name="startGfGenFrom">С какого элемента поля Галуа начинать строить кодирующий многочлен.</param>
        ''' <remarks>Некоторые источники начинают с 0, некоорые - с 1. Это число "b" в литературе.</remarks>
        ''' <returns>G(x) = (x-2^1) (x-2^2) (x-2^3)...(x-2^2t), 2t - число FEC символов.</returns>
        Public Function GenerateEncodingPolynomial(ByVal fecSymbolsNumber As Integer, Optional ByVal startGfGenFrom As Integer = 0) As Integer()
            Dim encPoly As Integer() = {1}
            For i As Integer = 0 + startGfGenFrom To fecSymbolsNumber - 1 + startGfGenFrom
                Dim power As Integer = Me.GaloisPow(2, i)
                encPoly = Me.PolynomialMultiplication(encPoly, {1, power})
            Next
            Return encPoly
        End Function

        ''' <summary>
        ''' Умножает два числа без переноса.
        ''' </summary>
        ''' <param name="x">Первый множитель.</param>
        ''' <param name="y">Второй множитель.</param>
        Private Function CarrylessMult(ByVal x As Integer, ByVal y As Integer) As Integer
            Dim product As Integer = 0
            Dim i As Integer = 0
            Do While (y >> i) > 0
                Dim sh As Integer = (1 << i)
                If ((y And sh) = sh) Then
                    product = product Xor (x << i)
                End If
                i += 1
            Loop
            Return product
        End Function

        ''' <summary>
        ''' Делит два числа по правилам двоичной арифметики без переносов и возвращает остаток.
        ''' </summary>
        ''' <param name="dividend">Делимое.</param>
        ''' <param name="divisor">Делитель.</param>
        Private Function CarrylessDivision(ByVal dividend As Integer, ByVal divisor As Integer) As Integer
            'Вычисляет позицию MSB для каждого числа: 
            Dim dividendLength As Integer = GetBitLength(dividend)
            Dim divisorLength As Integer = GetBitLength(divisor)

            If (dividendLength < divisorLength) Then
                'Если делимое меньше делителя, то возвращаем делимое:
                Return dividend
            Else
                'Иначе - выравниваем MSB делителя, равный "1", по MSB делимого, равный "1" (сдвигая делитель):
                For i As Integer = dividendLength - divisorLength To 0 Step -1
                    'Проверяем, что делимое можно разделить (неважно для первой итерации, но важно в последующих):
                    Dim sh As Integer = (1 << (i + divisorLength - 1))
                    If ((dividend And sh) = sh) Then
                        'Если деление возможно, сдвигаем делитель чтобы выровнять MSB, а затем XOR'им (вычитание без переноса):
                        dividend = dividend Xor (divisor << i)
                    End If
                Next
            End If
            Return dividend
        End Function

        ''' <summary>
        ''' Возвращает позицию наиболее значимого бита числа, равного "1".
        ''' </summary>
        ''' <param name="number">Число, для которого нужно определить позицию MSB "1".</param>
        Private Function GetBitLength(ByVal number As Integer) As Integer
            Dim bits As Integer
            Do While ((number >> bits) > 0)
                bits += 1
            Loop
            Return bits
        End Function

#End Region '/CLOSED METHODS

    End Class '/GaloisField

End Namespace 'RS