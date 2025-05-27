{-# LANGUAGE FlexibleContexts #-}

module Semantic where

import Data.Maybe (isJust)
import qualified Data.Map as Map
import Control.Monad.State
import Control.Monad.Except
import Control.Monad (when, unless, foldM)
import qualified AST as AST

-- Типы для семантического анализа
data SemType = SemInteger 
             | SemReal
             | SemBoolean
             | SemPointer TypeName   -- Указатель на именованный тип
             | SemRecord TypeName (Maybe TypeName) [FieldInfo]  -- Запись с (возможным) родителем и полями
             deriving (Eq, Show)

-- Информация о поле записи
data FieldInfo = FieldInfo {
    fieldName :: String,   -- Имя поля
    fieldSemType :: SemType  -- Тип поля
} deriving (Eq, Show)

-- Тип для имени типа
type TypeName = String

-- Таблица символов
type SymbolTable = Map.Map String SemType

-- Контекст семантического анализатора
data SemContext = SemContext {
    typeTable :: Map.Map TypeName SemType,  -- Таблица типов
    varTable :: SymbolTable  -- Таблица переменных
} deriving (Show)

-- Тип для обработки ошибок
type SemResult = ExceptT SemError (State SemContext)

-- Семантические ошибки
data SemError = 
    TypeNotDefined { pos :: (Int, Int), typeName :: String } |
    VarNotDefined { pos :: (Int, Int), varName :: String } |
    FieldNotDefined { pos :: (Int, Int), recordType :: String, errorFieldName :: String } |
    TypeMismatch { pos :: (Int, Int), expected :: SemType, actual :: SemType } |
    IncompatibleTypes { pos :: (Int, Int), op :: String, leftType :: SemType, rightType :: SemType } |
    NotRecordType { pos :: (Int, Int), actualType :: String } |
    NotPointerType { pos :: (Int, Int), actualType :: String } |
    NoInheritanceRelation { pos :: (Int, Int), sourceType :: String, targetType :: String } |
    CannotDereference { pos :: (Int, Int), actualType :: String } |
    LogicalExprExpected { pos :: (Int, Int), actualExprType :: SemType }
    deriving (Show)

-- Функция для проверки типов выражения
checkExpr :: SemLocated AST.Expression -> SemResult SemType
checkExpr (SemLocated pos expr) = case expr of
    AST.SimpleExpr simpleExpr -> checkSimpleExpr (SemLocated pos simpleExpr)
    AST.Relation leftExpr rel rightExpr -> do
        leftType <- checkSimpleExpr (SemLocated pos leftExpr)
        rightType <- checkSimpleExpr (SemLocated pos rightExpr)
        
        -- Проверяем совместимость типов для отношений
        case (leftType, rightType) of
            (SemInteger, SemInteger) -> return SemBoolean
            (SemReal, SemReal) -> return SemBoolean
            (SemInteger, SemReal) -> return SemBoolean
            (SemReal, SemInteger) -> return SemBoolean
            (SemBoolean, SemBoolean) -> 
                case rel of 
                    AST.Equal -> return SemBoolean
                    AST.NotEqual -> return SemBoolean
                    _ -> throwError $ IncompatibleTypes pos (show rel) leftType rightType
            (SemPointer _, SemPointer _) -> 
                case rel of
                    AST.Equal -> return SemBoolean
                    AST.NotEqual -> return SemBoolean
                    _ -> throwError $ IncompatibleTypes pos (show rel) leftType rightType
            _ -> throwError $ IncompatibleTypes pos (show rel) leftType rightType

-- Проверка простого выражения
checkSimpleExpr :: SemLocated AST.SimpleExpression -> SemResult SemType
checkSimpleExpr (SemLocated pos (AST.SimpleExpression sign terms)) = do
    -- Если есть знак, то выражение должно быть числовым
    when (isJust sign) $ do
        termTypes <- mapM (checkTermWithOp . SemLocated pos) terms
        unless (all isNumeric termTypes) $
            throwError $ TypeMismatch pos (SemReal) (head termTypes)
    
    -- Проверка всех термов
    termTypes <- mapM (checkTermWithOp . SemLocated pos) terms
    
    -- Если нет термов, возвращаем целый тип по умолчанию
    if null termTypes
        then return SemInteger
        else do
            -- Проверка совместимости типов термов
            let resultType = foldl promoteTypes (head termTypes) (tail termTypes)
            unless (isValidExprType termTypes) $ 
                if length termTypes >= 2 then
                    throwError $ IncompatibleTypes pos "+" (head termTypes) (termTypes !! 1)
                else 
                    throwError $ TypeMismatch pos SemBoolean (head termTypes)
            
            return resultType
    where
        isNumeric SemInteger = True
        isNumeric SemReal = True
        isNumeric _ = False
        
        isValidExprType termTypes =
            all isNumeric termTypes || all (== SemBoolean) termTypes
            
        -- Функция для автоматического повышения типов
        promoteTypes SemInteger SemReal = SemReal
        promoteTypes SemReal SemInteger = SemReal
        promoteTypes t1 t2 
            | t1 == t2 = t1
            | otherwise = t1  -- В случае ошибки просто возвращаем первый тип

-- Проверка терма с операцией
checkTermWithOp :: SemLocated AST.TermWithOp -> SemResult SemType
checkTermWithOp (SemLocated pos (AST.TermWithOp term op)) = do
    termType <- checkTerm (SemLocated pos term)
    
    case op of
        Nothing -> return termType
        Just AST.Add -> ensureNumericOrBoolean termType
        Just AST.Subtract -> ensureNumeric termType
        Just AST.Or -> ensureBoolean termType
    
    where
        ensureNumeric t 
            | t == SemInteger || t == SemReal = return t
            | otherwise = throwError $ TypeMismatch pos SemReal t
            
        ensureNumericOrBoolean t
            | t == SemInteger || t == SemReal || t == SemBoolean = return t
            | otherwise = throwError $ TypeMismatch pos SemReal t
            
        ensureBoolean t 
            | t == SemBoolean = return t
            | otherwise = throwError $ TypeMismatch pos SemBoolean t

-- Проверка терма
checkTerm :: SemLocated AST.Term -> SemResult SemType
checkTerm (SemLocated pos (AST.Term factor restFactors)) = do
    factorType <- checkFactor (SemLocated pos factor)
    
    -- Проверяем все дополнительные факторы
    foldM checkRestFactor factorType restFactors
    where
        checkRestFactor accType (op, nextFactor) = do
            nextType <- checkFactor (SemLocated pos nextFactor)
            
            case op of
                AST.Multiply -> ensureNumeric accType nextType
                AST.Divide -> do 
                    _ <- ensureNumeric accType nextType
                    return SemReal  -- Деление всегда дает вещественное число
                AST.Div -> ensureInteger accType nextType
                AST.Mod -> ensureInteger accType nextType
                AST.And -> ensureBoolean accType nextType
        
        ensureNumeric t1 t2
            | (t1 == SemInteger || t1 == SemReal) && 
              (t2 == SemInteger || t2 == SemReal) = 
                if t1 == SemReal || t2 == SemReal then return SemReal else return SemInteger
            | otherwise = throwError $ IncompatibleTypes pos "*" t1 t2
        
        ensureInteger t1 t2
            | t1 == SemInteger && t2 == SemInteger = return SemInteger
            | otherwise = throwError $ IncompatibleTypes pos "DIV" t1 t2
        
        ensureBoolean t1 t2
            | t1 == SemBoolean && t2 == SemBoolean = return SemBoolean
            | otherwise = throwError $ IncompatibleTypes pos "&" t1 t2

-- Проверка фактора
checkFactor :: SemLocated AST.Factor -> SemResult SemType
checkFactor (SemLocated pos factor) = case factor of
    AST.DesignatorFactor designator -> checkDesignator (SemLocated pos designator)
    AST.IntLiteral _ -> return SemInteger
    AST.RealLiteral _ -> return SemReal
    AST.ParenExpression expr -> checkExpr (SemLocated pos expr)
    AST.NotFactor innerFactor -> do
        innerType <- checkFactor (SemLocated pos innerFactor)
        unless (innerType == SemBoolean) $
            throwError $ TypeMismatch pos SemBoolean innerType
        return SemBoolean

-- Проверка пути доступа (идентификатора или поля)
checkDesignator :: SemLocated AST.Designator -> SemResult SemType
checkDesignator (SemLocated pos (AST.Designator baseName selectors)) = do
    -- Получаем тип базовой переменной
    ctx <- get
    baseType <- case Map.lookup baseName (varTable ctx) of
        Just t -> return t
        Nothing -> throwError $ VarNotDefined pos baseName
    
    -- Применяем все селекторы
    foldM applySelector baseType selectors
    where
        applySelector currentType selector = case selector of
            AST.FieldSelector fieldName -> case currentType of
                SemRecord typeName _ fields -> 
                    case findField fields fieldName of
                        Just fieldType -> return fieldType
                        Nothing -> throwError $ FieldNotDefined pos typeName fieldName
                _ -> throwError $ NotRecordType pos (show currentType)
            AST.Dereference -> case currentType of
                SemPointer targetType -> do
                    ctx <- get
                    case Map.lookup targetType (typeTable ctx) of
                        Just t -> return t
                        Nothing -> throwError $ TypeNotDefined pos targetType
                _ -> throwError $ CannotDereference pos (show currentType)
        
        -- Функция для поиска поля в записи
        findField fields name = 
            case filter (\f -> fieldName f == name) fields of
                (field:_) -> Just (fieldSemType field)
                [] -> Nothing

-- Проверка оператора присваивания
checkAssignment :: SemLocated AST.Statement -> SemResult ()
checkAssignment (SemLocated pos (AST.Assignment target expr)) = do
    targetType <- checkDesignator (SemLocated pos target)
    exprType <- checkExpr (SemLocated pos expr)
    
    -- Проверка совместимости типов
    areAssignable <- checkTypesAssignable targetType exprType
    unless areAssignable $
        throwError $ TypeMismatch pos targetType exprType
checkAssignment _ = throwError $ TypeMismatch (0,0) SemInteger SemInteger  -- Этот случай не должен возникать

-- Проверка совместимости типов для присваивания
checkTypesAssignable :: SemType -> SemType -> SemResult Bool
checkTypesAssignable SemInteger SemInteger = return True
checkTypesAssignable SemReal SemReal = return True
checkTypesAssignable SemReal SemInteger = return True  -- Целое может быть присвоено вещественному
checkTypesAssignable SemBoolean SemBoolean = return True

-- Записи точно одного типа всегда совместимы
checkTypesAssignable (SemRecord name1 _ _) (SemRecord name2 _ _) | name1 == name2 = return True

-- Указатели совместимы, если имеют иерархическую связь
checkTypesAssignable (SemPointer t1) (SemPointer t2) = checkTypeInheritance t2 t1

-- Записи совместимы, если имеют иерархическую связь (для разных типов)
checkTypesAssignable r1@(SemRecord t1 _ _) r2@(SemRecord t2 _ _) = 
    if t1 /= t2 then checkTypeInheritance t2 t1 else return True

checkTypesAssignable _ _ = return False

-- Функция проверки иерархической связи типов
checkTypeInheritance :: String -> String -> SemResult Bool
checkTypeInheritance typeName parentName
    | typeName == parentName = return True  -- Одинаковые типы
    | otherwise = do
        -- Проверяем отношение наследования между typeName и parentName
        -- typeName может быть присвоен parentName, если typeName является наследником parentName
        ctx <- get
        let checkInheritance tName pName =
                case Map.lookup tName (typeTable ctx) of
                    Just (SemRecord _ (Just p) _) | p == pName -> True
                    Just (SemRecord _ (Just p) _) -> checkInheritance p pName
                    _ -> False
        return $ checkInheritance typeName parentName

-- Проверка условия в IF и WHILE
checkCondition :: SemLocated AST.Expression -> SemResult ()
checkCondition (SemLocated pos expr) = do
    condType <- checkExpr (SemLocated pos expr)
    unless (condType == SemBoolean) $
        throwError $ LogicalExprExpected pos condType

-- Проверка оператора NEW
checkNew :: SemLocated AST.Statement -> SemResult ()
checkNew (SemLocated pos (AST.NewStatement target)) = do
    targetType <- checkDesignator (SemLocated pos target)
    case targetType of
        SemPointer _ -> return ()
        _ -> throwError $ NotPointerType pos (show targetType)
checkNew _ = throwError $ NotPointerType (0,0) "Invalid statement type"  -- Этот случай не должен возникать

-- Проверка statements
checkStatement :: SemLocated AST.Statement -> SemResult ()
checkStatement located@(SemLocated pos stmt) = case stmt of
    AST.Assignment {} -> checkAssignment located
    AST.IfStatement cond thenBlock elseBlock -> do
        -- Проверяем, что условие - это выражение сравнения или логическое выражение
        condType <- checkExpr (SemLocated pos cond)
        unless (condType == SemBoolean) $
            throwError $ LogicalExprExpected pos condType
        mapM_ (checkStatement . SemLocated pos) thenBlock
        maybe (return ()) (mapM_ (checkStatement . SemLocated pos)) elseBlock
    AST.WhileStatement cond block -> do
        -- Проверяем, что условие - это выражение сравнения или логическое выражение
        condType <- checkExpr (SemLocated pos cond)
        unless (condType == SemBoolean) $
            throwError $ LogicalExprExpected pos condType
        mapM_ (checkStatement . SemLocated pos) block
    AST.NewStatement {} -> checkNew located

-- Инициализация контекста семантического анализа
initContext :: AST.Program -> SemContext
initContext prog = SemContext {
    typeTable = buildTypeTable prog,
    varTable = buildVarTable prog
}

-- Построение таблицы типов
buildTypeTable :: AST.Program -> Map.Map TypeName SemType
buildTypeTable prog = 
    -- Сначала добавляем базовые типы
    let baseTypes = Map.fromList [
                      ("INTEGER", SemInteger),
                      ("REAL", SemReal),
                      ("BOOLEAN", SemBoolean)
                    ]
        
        -- Добавляем объявленные типы (пока без разрешения полей)
        declaredTypes = foldr addTypeDecl baseTypes (AST.typeDeclarations prog)
        
        -- Теперь обходим все типы и разрешаем их поля
        resolvedTypes = Map.mapWithKey (resolveType declaredTypes) declaredTypes
    in
        resolvedTypes
    where
        -- Добавление объявления типа в таблицу
        addTypeDecl :: AST.TypeDeclaration -> Map.Map TypeName SemType -> Map.Map TypeName SemType
        addTypeDecl (AST.TypeDeclaration name typeDef) typeMap =
            Map.insert name (convertType typeMap typeDef) typeMap
        
        -- Преобразование AST-типа в семантический тип
        convertType :: Map.Map TypeName SemType -> AST.Type -> SemType
        convertType _ AST.RealType = SemReal
        convertType _ AST.IntegerType = SemInteger
        convertType typeMap (AST.PointerType t) = 
            -- Для указателя нам нужно только получить имя типа
            case t of
                AST.NamedType name -> SemPointer name
                _ -> error "Pointer must point to a named type"  -- Это не должно произойти для правильного AST
        convertType typeMap (AST.RecordType parentName fields) =
            -- Для записи мы создаем предварительную запись без разрешенных полей
            SemRecord name parentName []
            where name = "ANONYMOUS_RECORD"  -- Для упрощения, но это можно улучшить
        convertType _ (AST.NamedType name) = 
            -- Именованный тип обрабатывается на этапе разрешения
            SemRecord name Nothing []
        
        -- Разрешение типов (заполнение полей записей)
        resolveType :: Map.Map TypeName SemType -> TypeName -> SemType -> SemType
        resolveType typeMap name semType@(SemRecord typeName parentName _) =
            case getTypeDecl name of
                Just (AST.RecordType parent fields) ->
                    -- Преобразуем поля и добавляем поля предка, если он есть
                    let ownFields = concatMap (convertField typeMap) fields
                        parentFields = case parent of
                                         Nothing -> []
                                         Just pName -> 
                                             case Map.lookup pName typeMap of
                                                 Just (SemRecord _ _ pFields) -> pFields
                                                 _ -> []
                    in
                        SemRecord name parent (parentFields ++ ownFields)
                _ -> semType
        resolveType _ _ semType = semType
        
        -- Получение объявления типа по имени
        getTypeDecl :: String -> Maybe AST.Type
        getTypeDecl name = 
            case filter (\td -> AST.typeName td == name) (AST.typeDeclarations prog) of
                (AST.TypeDeclaration _ typeDef : _) -> Just typeDef
                [] -> Nothing
        
        -- Преобразование объявления поля
        convertField :: Map.Map TypeName SemType -> AST.FieldDeclaration -> [FieldInfo]
        convertField typeMap (AST.FieldDeclaration names fieldType) =
            let semType = case fieldType of
                            AST.RealType -> SemReal
                            AST.IntegerType -> SemInteger
                            AST.PointerType (AST.NamedType name) -> SemPointer name
                            AST.NamedType name -> 
                                case Map.lookup name typeMap of
                                    Just t -> t
                                    Nothing -> error $ "Type not found: " ++ name
                            _ -> error "Unsupported field type"
            in
                map (\n -> FieldInfo n semType) names

-- Построение таблицы переменных
buildVarTable :: AST.Program -> Map.Map String SemType
buildVarTable prog =
    -- Добавляем объявленные переменные
    let typeMap = buildTypeTable prog
    in
        foldr (addVarDecl typeMap) Map.empty (AST.varDeclarations prog)
    where
        -- Добавление объявления переменной в таблицу
        addVarDecl :: Map.Map TypeName SemType -> AST.VarDeclaration -> Map.Map String SemType -> Map.Map String SemType
        addVarDecl typeMap (AST.VarDeclaration names varType) varMap =
            let semType = convertVarType typeMap varType
            in
                foldr (\name -> Map.insert name semType) varMap names
        
        -- Преобразование типа переменной в семантический тип
        convertVarType :: Map.Map TypeName SemType -> AST.Type -> SemType
        convertVarType _ AST.RealType = SemReal
        convertVarType _ AST.IntegerType = SemInteger
        convertVarType _ (AST.PointerType (AST.NamedType name)) = SemPointer name
        convertVarType typeMap (AST.NamedType name) = 
            case Map.lookup name typeMap of
                Just t -> t
                Nothing -> error $ "Type not found: " ++ name
        convertVarType _ t = error $ "Unsupported variable type: " ++ show t

-- Главная функция семантического анализа
analyzeProgram :: AST.Program -> Either SemError ()
analyzeProgram prog = 
    let initialContext = initContext prog
        stmts = AST.statements prog
        -- Присваиваем каждому оператору его последовательный номер вместо (0,0)
        -- в реальном коде с полной интеграцией с парсером мы бы получали позиции из AST
        stmtsWithPos = zipWith (\i stmt -> SemLocated (i, 0) stmt) [1..] stmts
        
        -- Анализируем каждый оператор отдельно для более точной диагностики
        analyzeStatements [] = Right ()
        analyzeStatements (stmt:rest) = 
            case runState (runExceptT (checkStatement stmt)) initialContext of
                (Left err, _) -> Left err
                (Right _, _) -> analyzeStatements rest
                
    in analyzeStatements stmtsWithPos

-- Добавление позиционной информации к AST для сообщений об ошибках
data SemLocated a = SemLocated { loc :: (Int, Int), unLoc :: a } 