Imports System.Text

Namespace RS

    ''' <summary>
    ''' Кодовое слово и методы для декодирования кодовых слов.
    ''' </summary>
    Public Class EncodedCodeword

#Region "CTOR"

        ''' <summary>
        ''' Создаёт экземпляр кодового слова, определяет синдром и пытается восстановить исходное сообщение.
        ''' </summary>
        ''' <param name="gf">Поле Галуа, в котором рассчитывается данное сообщение.</param>
        ''' <param name="codeword">Кодовое слово = закодированное сообщение + FEC символы.</param>
        ''' <param name="fecNum">Количество FEC-символов в кодовом слове.</param>
        Public Sub New(ByVal gf As GaloisField, ByVal codeword As Integer(), ByVal fecNum As Integer)
            _GF = gf
            _OriginalCodeWord = codeword
            _FecSymbolsNumber = fecNum
            _Syndromes = CalculateSyndromes(OriginalCodeWord, FecSymbolsNumber)

            If (Not ReceivedMessageCorrupted) Then
                _CorrectedCodeWord = OriginalCodeWord
            Else
                _ErrorLocatorPolynomial = FindErrorLocator(Syndromes, fecNum)
                ErasurePositions = FindErrorPositions(ErrorLocatorPolynomial.Reverse.ToArray(), codeword.Count)
                _CorrectedCodeWord = CorrectErrata(OriginalCodeWord, Syndromes, ErasurePositions.ToArray())
                'TODO Сделать проверку синдромов ещё раз по факту коррекции ошибок.
            End If
        End Sub

        ''' <summary>
        ''' Создаёт экземпляр кодового слова, определяет синдром и пытается восстановить исходное сообщение.
        ''' </summary>
        ''' <param name="gf">Поле Галуа, в котором рассчитывается данное сообщение.</param>
        ''' <param name="codeword">Кодовое слово = закодированное сообщение + FEC символы.</param>
        ''' <param name="fecNum">Количество FEC-символов в кодовом слове.</param>
        ''' <param name="erasurePositions">Массив известных позиций ошибочных символов. Например: {0,3,4,7}.</param>
        Public Sub New(ByVal gf As GaloisField, ByVal codeword As Integer(), ByVal fecNum As Integer, ByVal erasurePositions As Integer())
            _GF = gf
            _OriginalCodeWord = codeword
            _FecSymbolsNumber = fecNum
            _Syndromes = CalculateSyndromes(OriginalCodeWord, FecSymbolsNumber)

            _ErasurePositions = erasurePositions

            If (Not ReceivedMessageCorrupted) Then
                _CorrectedCodeWord = OriginalCodeWord
            Else
                _CorrectedCodeWord = CorrectErrata(OriginalCodeWord, Syndromes, Me.ErasurePositions.ToArray())
                'TODO Сделать проверку синдромов ещё раз по факту коррекции ошибок.
            End If
        End Sub

#End Region '/CTOR

#Region "PROPS"

        ''' <summary>
        ''' Поле Галуа, в котором декодируется данное сообщение.
        ''' </summary>
        Public ReadOnly Property GF As GaloisField
            Get
                Return _GF
            End Get
        End Property
        Private _GF As GaloisField

        ''' <summary>
        ''' Оригинальное кодовое слово (сообщение + FEC символы).
        ''' </summary>
        Public ReadOnly Property OriginalCodeWord As Integer()
            Get
                Return _OriginalCodeWord
            End Get
        End Property
        Private _OriginalCodeWord As Integer() = New Integer() {}

        ''' <summary>
        ''' Число корректирующих FEC-символов.
        ''' </summary>
        Public ReadOnly Property FecSymbolsNumber As Integer
            Get
                Return _FecSymbolsNumber
            End Get
        End Property
        Private _FecSymbolsNumber As Integer

        ''' <summary>
        ''' Исходное сообщение (без FEC-символов).
        ''' </summary>
        Public ReadOnly Property OriginalMessage As Integer()
            Get
                Dim len As Integer = OriginalCodeWord.Length - FecSymbolsNumber - 1
                Dim msg(len - 1) As Integer
                Array.Copy(OriginalCodeWord, msg, len)
                Return msg
            End Get
        End Property

        ''' <summary>
        ''' Исходные (восстановленные) FEC символы.
        ''' </summary>
        Public ReadOnly Property OriginalFec As Integer()
            Get
                Dim fec(FecSymbolsNumber - 1) As Integer
                Array.Copy(OriginalMessage, OriginalMessage.Length - FecSymbolsNumber, fec, 0, FecSymbolsNumber)
                Return fec
            End Get
        End Property

        ''' <summary>
        ''' Исправленное кодовое слово (сообщение + FEC символы).
        ''' </summary>
        Public ReadOnly Property CorrectedCodeWord As Integer()
            Get
                Return _CorrectedCodeWord
            End Get
        End Property
        Private _CorrectedCodeWord As Integer() = New Integer() {}

        ''' <summary>
        ''' Скорректированное (восстановленное) сообщение.
        ''' </summary>
        Public ReadOnly Property CorrectedMessage As Integer()
            Get
                Dim msg() As Integer = New Integer() {}
                Dim len As Integer = CorrectedCodeWord.Length - FecSymbolsNumber
                If (len > 0) Then
                    ReDim msg(len - 1)
                    Array.Copy(CorrectedCodeWord, msg, len)
                End If
                Return msg
            End Get
        End Property

        ''' <summary>
        ''' Восстановленные корректирующие символы.
        ''' </summary>
        Public ReadOnly Property CorrectedFec As Integer()
            Get
                Dim fec() As Integer = New Integer() {}
                If (CorrectedCodeWord.Count > 0) Then
                    ReDim fec(FecSymbolsNumber - 1)
                    Array.Copy(CorrectedCodeWord, CorrectedCodeWord.Length - FecSymbolsNumber, fec, 0, FecSymbolsNumber)
                End If
                Return fec
            End Get
        End Property

        ''' <summary>
        ''' Многочлен "синдрома" полученного сообщения, которое предстоит дешифрировать.
        ''' </summary>
        Public ReadOnly Property Syndromes As Integer()
            Get
                Return _Syndromes
            End Get
        End Property
        Private _Syndromes As Integer() = New Integer() {}

        ''' <summary>
        ''' Показывает, что в принятом сообщении содержатся ошибки.
        ''' </summary>
        ''' <remarks>Перед обращением к свойству должен быть вызван метод Decode(), чтобы обновить значения синдромов.</remarks>
        Public ReadOnly Property ReceivedMessageCorrupted As Boolean
            Get
                Return (Syndromes.Max > 0)
            End Get
        End Property

        ''' <summary>
        ''' Многочлен положений ошибок/пропусков в принятом сообщении Лямбда Λ(x).
        ''' </summary>
        Public ReadOnly Property ErrorLocatorPolynomial As Integer()
            Get
                Return _ErrorLocatorPolynomial
            End Get
        End Property
        Private _ErrorLocatorPolynomial As Integer() = New Integer() {}

        ''' <summary>
        ''' Многочлен определитель ошибок в принятом сообщении Омега Ω(x).
        ''' </summary>
        Public ReadOnly Property ErrorEvaluatorPolynomial As Integer()
            Get
                Return _ErrorEvaluatorPolynomial
            End Get
        End Property
        Private _ErrorEvaluatorPolynomial As Integer() = New Integer() {}

        ''' <summary>
        ''' Многочлен ошибок Xj().
        ''' </summary>
        Public ReadOnly Property Xj As Integer()
            Get
                Return _Xj
            End Get
        End Property
        Private _Xj As Integer() = New Integer() {}

        ''' <summary>
        ''' Многочлен магнитуд Yj() для коррекции ошибок.
        ''' </summary>
        Public ReadOnly Property MagnitudePolynomial As Integer()
            Get
                Return _MagnitudePolynomial
            End Get
        End Property
        Private _MagnitudePolynomial As Integer() = New Integer() {}

        ''' <summary>
        ''' Массив позиций ошибочных символов в кодовом слове. Нумерация начинается с нуля.
        ''' </summary>
        Public Property ErasurePositions As Integer()
            Get
                Return _ErasurePositions
            End Get
            Set(value As Integer())
                If (Not _ErasurePositions.Equals(value)) Then
                    _ErasurePositions = value
                End If
            End Set
        End Property
        Private _ErasurePositions As Integer() = New Integer() {}

        ''' <summary>
        ''' Результат анализа декодированного сообщения (число найденных и восставновленных ошибок и т.д.).
        ''' </summary>
        Public ReadOnly Property DecodingStatus As String
            Get
                Dim status As New StringBuilder()

                'Синдромы и признак ошибки:
                If (Syndromes.Length > 0) Then
                    status.Append($"Синдром: {ReedSolomon.GetPolynomialAsString(Syndromes, , False, , "S(x)")} => ")
                    status.Append("Ошибки в сообщении: ")
                    If ReceivedMessageCorrupted Then
                        status.AppendLine(String.Format("Да."))
                    Else
                        status.AppendLine(String.Format("Нет."))
                    End If
                End If

                If (ErrorLocatorPolynomial.Length > 0) Then
                    status.AppendLine($"Многочлен-локатор ошибок: {ReedSolomon.GetPolynomialAsString(ErrorLocatorPolynomial, , False, , "Λ(x)")}")
                End If

                If (ErrorEvaluatorPolynomial.Length > 0) Then
                    status.AppendLine($"Многочлен-определитель ошибок: {ReedSolomon.GetPolynomialAsString(ErrorEvaluatorPolynomial, , False, , "Ω(x)")}")
                End If

                If (Xj.Length > 0) Then
                    status.AppendLine($"Многочлен ошибок: {ReedSolomon.GetPolynomialAsString(Xj, , False, , "X(j)")}")
                End If

                'TODO Как точно называется?
                If (ErasurePositions.Length > 0) AndAlso (ReceivedMessageCorrupted) Then
                    status.AppendLine($"Ошибки на позициях: {ReedSolomon.GetPolynomialAsString(ErasurePositions, , False, , "L(x)")}")
                End If

                If (MagnitudePolynomial.Length > 0) Then
                    status.AppendLine($"Многочлен магнитуд {ReedSolomon.GetPolynomialAsString(MagnitudePolynomial, , False, , "Y(j)")}")
                End If

                'Исправленное сообщение:
                If (CorrectedMessage.Length > 0) Then
                    status.AppendLine($"Исправленное сообщение: {ReedSolomon.GetPolynomialAsString(CorrectedMessage, , False)}")
                End If

                'Исправленные FEC символы:
                If (CorrectedFec.Length > 0) Then
                    status.AppendLine($"Исправленные FEC символы: {ReedSolomon.GetPolynomialAsString(CorrectedFec, , False)}")
                End If
                Return status.ToString()
            End Get
        End Property

#End Region '/PROPS

#Region "ВОССТАНОВЛЕНИЕ СООБЩЕНИЯ"

        ''' <summary>
        ''' Вычисляет многочлен "синдромов" сообщения, используя только ошибки 
        ''' (т.е. ошибки - это символы, которые были пропущены или изменены, но мы не знаем их позиции).
        ''' </summary>
        ''' <param name="message">Многочлен сообщения, которое проверяется на ошибки.</param>
        ''' <param name="fecSymbolsNumber">Число FEC символов. (Эквивалент - "nmess").</param>
        ''' <returns>Массив целых чисел. Ненулевые элементы свидетельствуют о наличии ошибки.</returns>
        ''' <remarks>В конце добавлям к наименьшей степени коэффициент "0" (константа, без "Х"). 
        ''' Это сдвигает синдром, и сдвигает все расчёты, зависящие от синдрома 
        ''' (такие как многочлен положений ошибок/пропусков errors locator polynomial, оценочный многочлен errors evaluator polynomial и т.д., кроме позиций ошибок).
        ''' Это не обязательно, можно адаптировать промежуточные расчёты, начиная циклы с 0 вместо пропуска первой итерации
        ''' (т.е. часто встречающийся диапазон (1, FEC+1)).</remarks>
        Private Function CalculateSyndromes(ByVal message As Integer(), ByVal fecSymbolsNumber As Integer) As Integer()
            Dim syndromes(fecSymbolsNumber - 1) As Integer
            For i As Integer = 0 To fecSymbolsNumber - 1
                syndromes(i) = GF.PolynomialEval(message, GF.GaloisPow(2, i))
            Next
            Return syndromes
        End Function

#Region "ERASURE CORRECTION"

        ''' <summary>
        ''' Вычисляет магнитуды ошибок, восстанавливает и возвращает исходное кодовое слово.
        ''' </summary>
        ''' <param name="message">Принятое сообщение.</param>
        ''' <param name="syndromes">Синдромы.</param>
        ''' <param name="errPositions">Упорядоченный массив позиций ошибок, начиная с 0.</param>
        ''' <returns>Скорректированное слово (восстановленное сообщение + восстановленные FEC символы).</returns>
        Private Function CorrectErrata(ByVal message As Integer(), ByVal syndromes As Integer(), ByVal errPositions As Integer()) As Integer()

            'Находим полином-локатор ошибок Λ(x):
            Dim posFromEnd As New List(Of Integer)
            For Each p As Integer In errPositions
                posFromEnd.Add(message.Length - 1 - p)
            Next
            _ErrorLocatorPolynomial = FindErrorLocator(posFromEnd.ToArray())

            'Находим многочлен Xj:
            Dim x As New List(Of Integer)
            For i As Integer = 0 To posFromEnd.Count - 1
                x.Add(GF.ElementAt(posFromEnd(i)))
            Next
            _Xj = x.ToArray()

            'Находим полином-определитель ошибок Ω(x):
            Dim errEval As List(Of Integer) = FindErrorEvaluator(syndromes.Reverse.ToArray, ErrorLocatorPolynomial, ErrorLocatorPolynomial.Length - 1).ToList()
            Do While ((errEval.Count > 0) AndAlso (errEval(0) = 0))
                errEval.RemoveAt(0) 'Отбрасываем начальные нули.
            Loop
            _ErrorEvaluatorPolynomial = errEval.ToArray()

            'Находим магнитуды ошибок Y(j):
            Dim magnitudes(message.Length - 1) As Integer
            If (errPositions.Count = 1) Then
                Dim magnitude As Integer = syndromes(0)
                magnitudes(errPositions(0)) = magnitude

            ElseIf (errPositions.Count = 2) Then
                Dim a0 As Integer = GF.ElementAt(posFromEnd(0))
                Dim a1 As Integer = GF.ElementAt(posFromEnd(1))
                Dim dividend As Integer = GF.GaloisMultiply(a1, syndromes(0))
                dividend = GF.GaloisAddSubtract(dividend, syndromes(1))
                Dim divider As Integer = GF.GaloisAddSubtract(a0, a1)
                Dim magnitude0 As Integer = GF.GaloisDivide(dividend, divider)
                magnitudes(errPositions(0)) = magnitude0
                Dim magnitude1 As Integer = GF.GaloisAddSubtract(syndromes(0), magnitude0)
                magnitudes(errPositions(1)) = magnitude1

            Else
                'Алгоритм Форни - вычисление магнитуд:
                Dim forneyDividerPoly As New List(Of Integer)(ErrorLocatorPolynomial)
                For i As Integer = 0 To forneyDividerPoly.Count - 1
                    If (i Mod 2 = 0) Then
                        forneyDividerPoly(forneyDividerPoly.Count - 1 - i) = 0 'чётные степени обнуляем.
                    End If
                Next
                forneyDividerPoly.RemoveAt(forneyDividerPoly.Count - 1) 'сдвигаем на 1 разряд.

                For j As Integer = 0 To Xj.Count - 1
                    Dim xjInv As Integer = GF.GetMultiplicativeInverse(Xj(j))

                    Dim forneyDivider As Integer = GF.PolynomialEval(forneyDividerPoly.ToArray(), xjInv)
                    Dim forneyDividend As Integer = GF.PolynomialEval(ErrorEvaluatorPolynomial, xjInv)
                    forneyDividend = GF.GaloisMultiply(Xj(j), forneyDividend)

                    Dim magnitude As Integer = GF.GaloisDivide(forneyDividend, forneyDivider) 'магнитуда ошибок Y(j).
                    magnitudes(errPositions(j)) = magnitude
                Next
            End If

            'Применяем найденные значения магнитуд, чтобы скорректировть ошибки в сообщении (FEC символы также корректируются):
            _MagnitudePolynomial = magnitudes
            Dim correctedCodeword As Integer() = CorrectCodeword(message, MagnitudePolynomial)
            Return correctedCodeword
        End Function

        ''' <summary>
        ''' Вычисляет многочлен-локатор ошибок Лямбда Λ(x).
        ''' </summary>
        ''' <param name="erasurePositions">Обращённый многочлен с известными позициями ошибок 
        ''' Пример: "hello worldxxxxxxxxx" принятое как "h_ll_orldxxxxxxxxx". 
        ''' xxxxxxxxx - FEC символы длиной n-k=9, позиции ошибок [1,4]. 
        ''' Но к-ты будут отражены т.к. FEC символы размещаются как младшие к-ты многочлена, 
        ''' т.о. к-ты ошибочных символов (n-1)-{1,4} = {18,15}.</param>
        ''' <remarks>В английском переводе - Error Locator Polynomial.</remarks>
        Private Function FindErrorLocator(ByVal erasurePositions As Integer()) As Integer()
            Dim errLocator As Integer() = {1} 'нельзя инициализировать произведение нулём => "1".
            For Each i As Integer In erasurePositions
                Dim sum As Integer() = GF.PolynomialAddition({1}, {GF.GaloisPow(2, i), 0})
                errLocator = GF.PolynomialMultiplication(errLocator, sum)
            Next
            Return errLocator
        End Function

        ''' <summary>
        ''' Вычисляет оценочный многочлен Омега по многочлену синдромов и многочлену-локатору ошибок Сигма.
        ''' </summary>
        ''' <param name="syndromes">Многочлен синдромов.</param>
        ''' <param name="errataLocator">Многочлен локатор ошибок Сигма.</param>
        ''' <param name="fecSymbolsNumber">Число FEC символов.</param>
        Private Function FindErrorEvaluator(ByVal syndromes As Integer(), ByVal errataLocator As Integer(), ByVal fecSymbolsNumber As Integer) As Integer()
            Dim mul As Integer() = GF.PolynomialMultiplication(syndromes, errataLocator)
            Dim remainder As New List(Of Integer)(mul)
            Dim len As Integer = remainder.Count - (fecSymbolsNumber + 1)
            remainder.RemoveRange(0, len)
            Return remainder.ToArray()
        End Function

        ''' <summary>
        ''' Возвращает исправленное сообщение по многочлену магнитуд ошибок.
        ''' </summary>
        ''' <param name="message">Принятое сообщение с ошибками C'(x) = C(x) + E(x).</param>
        ''' <param name="magnitudes">Многочлен магнитуд ошибок E(x).</param>
        ''' <returns>Восстановленное сообщение с вычетом многочлена магнитуд ошибок: C(x) = C'(x) - E(x).</returns>
        Private Function CorrectCodeword(ByVal message() As Integer, ByVal magnitudes() As Integer) As Integer()
            Dim correctedCodeword As Integer() = GF.PolynomialAddition(message, MagnitudePolynomial)
            Return correctedCodeword
        End Function

#End Region '/ERASURE CORRECTION

#Region "ERROR CORRECTION"

        ''' <summary>
        ''' Вычисляет локатор ошибок (error/errata locator) и вычислитель многочленов (evaluator polynomials) по алгоритму Берлекампа-Месси.
        ''' </summary>
        ''' <param name="synd">Многочлен синдромов.</param>
        ''' <param name="fecSymbNumber">Число FEC символов.</param>
        ''' <param name="eraseLocator"></param>
        ''' <param name="eraseCount"></param>
        ''' <remarks>The idea is that BM will iteratively estimate the error locator polynomial. 
        ''' To do this, it will compute a Discrepancy term called Delta, which will tell us 
        ''' if the error locator polynomial needs an update or not 
        ''' (hence why it's called discrepancy: it tells us when we are getting off board from the correct value).</remarks>
        Private Function FindErrorLocator(ByVal synd As Integer(), ByVal fecSymbNumber As Integer,
                                              Optional ByVal eraseLocator As Integer() = Nothing, Optional eraseCount As Integer = 0) As Integer()
            'Инициализация многочленов:
            Dim errLocator As New List(Of Integer) 'основной искомый многочлен Сигма, или многочлен-локатор ошибок.
            Dim prevLocator As Integer() 'т.к. алгоритм итеративный, мы должны хранить значения предыдущей итерации.
            If (eraseLocator IsNot Nothing) AndAlso (eraseLocator.Length > 0) Then
                'Если есть многочлен-локатор, то инициализируем его значениями и т.о. включаем пропущенные символы в финальный многочлен-локатор:
                errLocator = New List(Of Integer)(eraseLocator)
                prevLocator = CType(eraseLocator.Clone(), Integer())
            Else
                errLocator = New List(Of Integer)({1})
                prevLocator = {1}
            End If

            'Fix the syndrome shifting: when computing the syndrome, some implementations may prepend a 0 coefficient 
            'for the lowest degree term (the constant). 
            'This is a case of syndrome shifting, thus the syndrome will be bigger than the number of ECC symbols 
            '(I don't know what purpose serves this shifting). 
            'If that's the case, then we need to account for the syndrome shifting when we use the syndrome such as inside BM, 
            'by skipping those prepended coefficients.
            'Another way to detect the shifting is to detect the 0 coefficients: by definition, 
            'a syndrome does not contain any 0 coefficient 
            '(except if there are no errors/erasures, in this case they are all 0). 
            'This however doesn't work with the modified Forney syndrome, which set to 0 the coefficients corresponding to erasures, 
            'leaving only the coefficients corresponding to errors.
            Dim syndromeShift As Integer = 0
            If (synd.Length > fecSymbNumber) Then
                syndromeShift = synd.Length - fecSymbNumber
            End If

            'Generally: nsym-erase_count == len(synd), except when you input a partial erase_loc and using the full syndrome instead of the Forney syndrome, in which case nsym-erase_count is more correct (len(synd) will fail badly with IndexError).
            For i As Integer = 0 To fecSymbNumber - eraseCount - 1
                Dim k As Integer = 0
                If (eraseLocator IsNot Nothing) AndAlso (eraseLocator.Length > 0) Then
                    'Если есть многочлен-локатор, нужно пропустить первую итерацию erase_count (но не последнюю, это очень важно!)
                    k = i + syndromeShift + eraseCount
                Else
                    'Если многочлена-локатора нет, then either there's no erasures to account or we use the Forney syndromes, so we don't need to use erase_count nor erase_loc (the erasures have been trimmed out of the Forney syndromes).
                    k = i + syndromeShift
                End If

                'Вычисление разности Дельта:
                'Here is the close-to-the-books operation to compute the discrepancy Delta: it's a simple polynomial multiplication of error locator with the syndromes, and then we get the Kth element.
                '  delta = gf_poly_mul(err_loc[::-1], synd)[K] # theoretically it should be gf_poly_add(synd[::-1], [1])[::-1] instead of just synd, but it seems it's not absolutely necessary to correctly decode.
                'But this can be optimized: since we only need the Kth element, we don't need to compute the polynomial multiplication for any other element but the Kth. Thus to optimize, we compute the polymul only at the item we need, skipping the rest (avoiding a nested loop, thus we are linear time instead of quadratic).
                'This optimization is actually described in several figures of the book "Algebraic codes for data transmission", Blahut, Richard E., 2003, Cambridge university press.
                Dim delta As Integer = synd(k) 'т.н. разница Дельта.
                For j As Integer = 1 To errLocator.Count - 1
                    'Here we do a partial polynomial multiplication (ie, we compute the polynomial multiplication only for the term of degree K).
                    Dim ind As Integer = errLocator.Count - (j + 1)
                    delta = delta Xor GF.GaloisMultiply(errLocator(ind), synd(k - j))
                Next

                'Сдвигаем многочлен, чтобы вычислить следующую степень: 
                ReDim Preserve prevLocator(prevLocator.Length)

                'Итеративно экстраполируем многочлен-локатор ошибок и многочлен-вычислитель:
                If (delta <> 0) Then 'Обновляем только при наличии разницы Дельта
                    If (prevLocator.Length > errLocator.Count) Then 'Rule B (rule A is implicitly defined because rule A just says that we skip any modification for this iteration)
                        'if 2*L <= K+erase_count: # equivalent to len(old_loc) > len(err_loc), as long as L is correctly computed
                        'Вычиляем многочлен-локатор ошибок Сигма:
                        Dim new_loc As Integer() = GF.PolynomialMultiplicationScalar(prevLocator, delta)
                        prevLocator = GF.PolynomialMultiplicationScalar(errLocator.ToArray(), GF.GetMultiplicativeInverse(delta)) 'Это эквивалентно: "err_loc * 1/delta = err_loc // delta".
                        errLocator = New List(Of Integer)(new_loc)
                        '# Update the update flag:
                        'L = K - L # the update flag L is tricky: in Blahut's schema, it's mandatory to use `L = K - L - erase_count` (and indeed in a previous draft of this function, if you forgot to do `- erase_count` it would lead to correcting only 2*(errors+erasures) <= (n-k) instead of 2*errors+erasures <= (n-k)), but in this latest draft, this will lead to a wrong decoding in some cases where it should correctly decode! Thus you should try with and without `- erase_count` to update L on your own implementation and see which one works OK without producing wrong decoding failures.
                    End If

                    'Обновляем разницу Дельта:
                    errLocator = New List(Of Integer)(GF.PolynomialAddition(errLocator.ToArray(), GF.PolynomialMultiplicationScalar(prevLocator, delta)))
                End If
            Next

            'Отбрасываем начальные нули:
            Do While ((errLocator.Count > 0) AndAlso (errLocator(0) = 0))
                errLocator.RemoveAt(0)
            Loop

            'Проверяем, что ошибок для коррекции не слишком много:
            Dim errs As Integer = errLocator.Count - 1
            If (((errs - eraseCount) + (errs - eraseCount) + eraseCount) > fecSymbNumber) Then
                Throw New Exception("Число ошибок превышает корректирующую способность кода.")
            End If

            Return errLocator.ToArray()
        End Function

        ''' <summary>
        ''' Ищет места ошибкок в сообщении, используя метод перебора (вместо поиска Чина).
        ''' </summary>
        ''' <param name="errorLocator">Многочлен-локатор ошибок.</param>
        ''' <param name="fecSymbNumber">Число FEC символов.</param>
        ''' <remarks>Ищет с помощью перебора корни полинома ошибок (где evaluation == zero). Это короткий, но менее эффективный, поиск Чина (Hchien's search).</remarks>
        Private Function FindErrorPositions(ByVal errorLocator As Integer(), ByVal fecSymbNumber As Integer) As Integer()
            Dim errPositions As New List(Of Integer)
            For i As Integer = 0 To fecSymbNumber - 1
                'Обычно нужно проверять все значения поля, но тут сделана оптимизация по проверке только интересующих значений:
                Dim eval As Integer = GF.PolynomialEval(errorLocator, GF.GaloisPow(2, i))
                If (eval = 0) Then 'Если корень = 0, это корень полинома-локатора ошибок, в противном случае это положение ошибки.
                    errPositions.Add(fecSymbNumber - 1 - i)
                End If
            Next

            'Число позиций ошибок должно совпадать с длиной многочлена-локатора ошибок:
            Dim chk As Boolean = (errPositions.Count = errorLocator.Length - 1)
            Debug.Assert(chk, "Число ошибок не соответствует длине многочлена-локатора ошибок.")
            'If (Not chk) Then
            '    Throw New Exception("Число ошибок не соответствует длине многочлена-локатора ошибок.")
            'End If
            errPositions.Reverse()
            Return errPositions.ToArray()
        End Function

#End Region '/ERROR CORRECTION

#Region "ERRATA CORRECTION"

        ''' <summary>
        ''' Вычисляет модифицированные синдромы Форни для определения только ошибок, но не пропусков символов (пропущенные символы обрезаются).
        ''' </summary>
        ''' <param name="synd">Оригинальные синдромы сообщения.</param>
        ''' <param name="pos">Позиции ошибок.</param>
        ''' <param name="nmess">Число FEC символов.</param>
        Private Function CalculateForneySyndromes(ByVal synd As Integer(), ByVal pos As Integer(), ByVal nmess As Integer) As Integer()
            'TEST Проверить генератор списка: "erase_pos_reversed = [nmess-1-p for p in pos]".
            Dim erase_pos_reversed As New List(Of Integer)
            For Each p As Integer In pos
                erase_pos_reversed.Add(nmess - 1 - p)
            Next

            'Array.Reverse(erase_pos_reversed, startReverseIndex, numElemsToReverse)
            Dim fsynd As New List(Of Integer)(synd)
            'fsynd.RemoveAt(0) 'обрезаем первый коэффициент, который всегда 0 по определению. 
            'NB Мы и не добавляли 0 в calculateSyndromes() => не обрезаем.

            'Оптимизированный метод расчёта синдромов Форни:
            For i As Integer = 0 To pos.Length - 1
                Dim x As Integer = GF.GaloisPow(2, erase_pos_reversed(i))
                For j As Integer = 0 To fsynd.Count - 2
                    fsynd(j) = GF.GaloisMultiply(fsynd(j), x) Xor fsynd(j + 1)
                Next
            Next

            Return fsynd.ToArray()
        End Function

#End Region '/ERRATA CORRECTION

#End Region '/ВОССТАНОВЛЕНИЕ СООБЩЕНИЯ

    End Class '/EncodedCodeword

End Namespace