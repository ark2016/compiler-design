module Semantic where

import Data.Maybe (isJust, fromJust)
import qualified Data.Map as Map
import Control.Monad.State
import Control.Monad.Except
import AST

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
    LogicalExprExpected { pos :: (Int, Int), actualType :: SemType }
    deriving (Show)

-- Функция для проверки типов выражения
checkExpr :: Located Expression -> SemResult SemType
checkExpr (Located pos expr) = case expr of
    SimpleExpr simpleExpr -> checkSimpleExpr (Located pos simpleExpr)
    Relation leftExpr rel rightExpr -> do
        leftType <- checkSimpleExpr (Located pos leftExpr)
        rightType <- checkSimpleExpr (Located pos rightExpr)
        
        -- Проверяем совместимость типов для отношений
        case (leftType, rightType) of
            (SemInteger, SemInteger) -> return SemBoolean
            (SemReal, SemReal) -> return SemBoolean
            (SemInteger, SemReal) -> return SemBoolean
            (SemReal, SemInteger) -> return SemBoolean
            (SemBoolean, SemBoolean) -> 
                if rel `elem` [Equal, NotEqual]
                then return SemBoolean
                else throwError $ IncompatibleTypes pos (show rel) leftType rightType
            (SemPointer _, SemPointer _) -> 
                if rel `elem` [Equal, NotEqual]
                then return SemBoolean
                else throwError $ IncompatibleTypes pos (show rel) leftType rightType
            _ -> throwError $ IncompatibleTypes pos (show rel) leftType rightType

-- Проверка простого выражения
checkSimpleExpr :: Located SimpleExpression -> SemResult SemType
checkSimpleExpr (Located pos (SimpleExpression sign terms)) = do
    -- Если есть знак, то выражение должно быть числовым
    when (isJust sign) $ do
        termTypes <- mapM (checkTermWithOp . Located pos) terms
        unless (all isNumeric termTypes) $
            throwError $ TypeMismatch pos (SemReal) (head termTypes)
    
    -- Проверка всех термов
    termTypes <- mapM (checkTermWithOp . Located pos) terms
    
    -- Если нет термов, возвращаем целый тип по умолчанию
    if null termTypes
        then return SemInteger
        else do
            -- Проверка совместимости типов термов
            let resultType = foldl promoteTypes (head termTypes) (tail termTypes)
            unless (isValidExprType termTypes) $
                throwError $ IncompatibleTypes pos "+" (head termTypes) (termTypes !! 1)
            
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
checkTermWithOp :: Located TermWithOp -> SemResult SemType
checkTermWithOp (Located pos (TermWithOp term op)) = do
    termType <- checkTerm (Located pos term)
    
    case op of
        Nothing -> return termType
        Just Add -> ensureNumericOrBoolean termType
        Just Subtract -> ensureNumeric termType
        Just Or -> ensureBoolean termType
    
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
checkTerm :: Located Term -> SemResult SemType
checkTerm (Located pos (Term factor restFactors)) = do
    factorType <- checkFactor (Located pos factor)
    
    -- Проверяем все дополнительные факторы
    foldM checkRestFactor factorType restFactors
    where
        checkRestFactor accType (op, nextFactor) = do
            nextType <- checkFactor (Located pos nextFactor)
            
            case op of
                Multiply -> ensureNumeric accType nextType
                Divide -> do 
                    ensureNumeric accType nextType
                    return SemReal  -- Деление всегда дает вещественное число
                Div -> ensureInteger accType nextType
                Mod -> ensureInteger accType nextType
                And -> ensureBoolean accType nextType
        
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
checkFactor :: Located Factor -> SemResult SemType
checkFactor (Located pos factor) = case factor of
    DesignatorFactor designator -> checkDesignator (Located pos designator)
    IntLiteral _ -> return SemInteger
    RealLiteral _ -> return SemReal
    ParenExpression expr -> checkExpr (Located pos expr)
    NotFactor innerFactor -> do
        innerType <- checkFactor (Located pos innerFactor)
        unless (innerType == SemBoolean) $
            throwError $ TypeMismatch pos SemBoolean innerType
        return SemBoolean

-- Проверка пути доступа (идентификатора или поля)
checkDesignator :: Located Designator -> SemResult SemType
checkDesignator (Located pos (Designator baseName selectors)) = do
    -- Получаем тип базовой переменной
    ctx <- get
    baseType <- case Map.lookup baseName (varTable ctx) of
        Just t -> return t
        Nothing -> throwError $ VarNotDefined pos baseName
    
    -- Применяем все селекторы
    foldM applySelector baseType selectors
    where
        applySelector currentType selector = case selector of
            FieldSelector fieldName -> case currentType of
                SemRecord typeName _ fields -> 
                    case findField fields fieldName of
                        Just fieldType -> return fieldType
                        Nothing -> throwError $ FieldNotDefined pos typeName errorFieldName
                _ -> throwError $ NotRecordType pos (show currentType)
            Dereference -> case currentType of
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
checkAssignment :: Located Statement -> SemResult ()
checkAssignment (Located pos (Assignment target expr)) = do
    targetType <- checkDesignator (Located pos target)
    exprType <- checkExpr (Located pos expr)
    
    -- Проверка совместимости типов
    areAssignable <- checkTypesAssignable targetType exprType
    unless areAssignable $
        throwError $ TypeMismatch pos targetType exprType

-- Проверка совместимости типов для присваивания
checkTypesAssignable :: SemType -> SemType -> SemResult Bool
checkTypesAssignable SemInteger SemInteger = return True
checkTypesAssignable SemReal SemReal = return True
checkTypesAssignable SemReal SemInteger = return True  -- Целое может быть присвоено вещественному
checkTypesAssignable SemBoolean SemBoolean = return True

-- Указатели совместимы, если имеют иерархическую связь
checkTypesAssignable (SemPointer t1) (SemPointer t2) = checkTypeInheritance t2 t1

-- Записи совместимы, если имеют иерархическую связь
checkTypesAssignable (SemRecord t1 _ _) (SemRecord t2 _ _) = checkTypeInheritance t2 t1

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
checkCondition :: Located Expression -> SemResult ()
checkCondition (Located pos expr) = do
    condType <- checkExpr (Located pos expr)
    unless (condType == SemBoolean) $
        throwError $ LogicalExprExpected pos condType

-- Проверка оператора NEW
checkNew :: Located Statement -> SemResult ()
checkNew (Located pos (NewStatement target)) = do
    targetType <- checkDesignator (Located pos target)
    case targetType of
        SemPointer _ -> return ()
        _ -> throwError $ NotPointerType pos (show targetType)

-- Проверка statements
checkStatement :: Located Statement -> SemResult ()
checkStatement located@(Located pos stmt) = case stmt of
    Assignment {} -> checkAssignment located
    IfStatement cond thenBlock elseBlock -> do
        checkCondition (Located pos cond)
        mapM_ (checkStatement . Located pos) thenBlock
        maybe (return ()) (mapM_ (checkStatement . Located pos)) elseBlock
    WhileStatement cond block -> do
        checkCondition (Located pos cond)
        mapM_ (checkStatement . Located pos) block
    NewStatement {} -> checkNew located

-- Инициализация контекста семантического анализа
initContext :: Program -> SemContext
initContext prog = SemContext {
    typeTable = buildTypeTable prog,
    varTable = buildVarTable prog
}

-- Построение таблицы типов
buildTypeTable :: Program -> Map.Map TypeName SemType
buildTypeTable prog = 
    -- Сначала добавляем базовые типы
    let baseTypes = Map.fromList [
                      ("INTEGER", SemInteger),
                      ("REAL", SemReal),
                      ("BOOLEAN", SemBoolean)
                    ]
        
        -- Добавляем объявленные типы (пока без разрешения полей)
        declaredTypes = foldr addTypeDecl baseTypes (typeDeclarations prog)
        
        -- Теперь обходим все типы и разрешаем их поля
        resolvedTypes = Map.mapWithKey (resolveType declaredTypes) declaredTypes
    in
        resolvedTypes
    where
        -- Добавление объявления типа в таблицу
        addTypeDecl :: TypeDeclaration -> Map.Map TypeName SemType -> Map.Map TypeName SemType
        addTypeDecl (TypeDeclaration name typeDef) typeMap =
            Map.insert name (convertType typeMap typeDef) typeMap
        
        -- Преобразование AST-типа в семантический тип
        convertType :: Map.Map TypeName SemType -> Type -> SemType
        convertType _ RealType = SemReal
        convertType _ IntegerType = SemInteger
        convertType typeMap (PointerType t) = 
            -- Для указателя нам нужно только получить имя типа
            case t of
                NamedType name -> SemPointer name
                _ -> error "Pointer must point to a named type"  -- Это не должно произойти для правильного AST
        convertType typeMap (RecordType parentName fields) =
            -- Для записи мы создаем предварительную запись без разрешенных полей
            SemRecord name parentName []
            where name = "ANONYMOUS_RECORD"  -- Для упрощения, но это можно улучшить
        convertType _ (NamedType name) = 
            -- Именованный тип обрабатывается на этапе разрешения
            SemRecord name Nothing []
        
        -- Разрешение типов (заполнение полей записей)
        resolveType :: Map.Map TypeName SemType -> TypeName -> SemType -> SemType
        resolveType typeMap name semType@(SemRecord typeName parentName _) =
            case getTypeDecl name of
                Just (RecordType parent fields) ->
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
        getTypeDecl :: String -> Maybe Type
        getTypeDecl name = 
            case filter (\td -> typeName td == name) (typeDeclarations prog) of
                (TypeDeclaration _ typeDef : _) -> Just typeDef
                [] -> Nothing
        
        -- Преобразование объявления поля
        convertField :: Map.Map TypeName SemType -> FieldDeclaration -> [FieldInfo]
        convertField typeMap (FieldDeclaration names fieldType) =
            let semType = case fieldType of
                            RealType -> SemReal
                            IntegerType -> SemInteger
                            PointerType (NamedType name) -> SemPointer name
                            NamedType name -> 
                                case Map.lookup name typeMap of
                                    Just t -> t
                                    Nothing -> error $ "Type not found: " ++ name
                            _ -> error "Unsupported field type"
            in
                map (\n -> FieldInfo n semType) names

-- Построение таблицы переменных
buildVarTable :: Program -> Map.Map String SemType
buildVarTable prog =
    -- Добавляем объявленные переменные
    let typeMap = buildTypeTable prog
    in
        foldr (addVarDecl typeMap) Map.empty (varDeclarations prog)
    where
        -- Добавление объявления переменной в таблицу
        addVarDecl :: Map.Map TypeName SemType -> VarDeclaration -> Map.Map String SemType -> Map.Map String SemType
        addVarDecl typeMap (VarDeclaration names varType) varMap =
            let semType = convertVarType typeMap varType
            in
                foldr (\name -> Map.insert name semType) varMap names
        
        -- Преобразование типа переменной в семантический тип
        convertVarType :: Map.Map TypeName SemType -> Type -> SemType
        convertVarType _ RealType = SemReal
        convertVarType _ IntegerType = SemInteger
        convertVarType _ (PointerType (NamedType name)) = SemPointer name
        convertVarType typeMap (NamedType name) = 
            case Map.lookup name typeMap of
                Just t -> t
                Nothing -> error $ "Type not found: " ++ name
        convertVarType _ t = error $ "Unsupported variable type: " ++ show t

-- Главная функция семантического анализа
analyzeProgram :: Program -> Either SemError ()
analyzeProgram prog = 
    let initialContext = initContext prog
        stmts = statements prog
        result = runState (runExceptT (mapM_ (checkStatement . Located (0,0)) stmts)) initialContext
    in
        case fst result of
            Left err -> Left err
            Right _ -> Right ()

-- Добавление позиционной информации к AST для сообщений об ошибках
data Located a = Located { loc :: (Int, Int), unLoc :: a } 