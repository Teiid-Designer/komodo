/*
 * JBoss, Home of Professional Open Source.
 *
 * See the LEGAL.txt file distributed with this work for information regarding copyright ownership and licensing.
 *
 * See the AUTHORS.txt file distributed with this work for a full listing of individual contributors.
 */
package org.komodo.relational.model.internal;

import java.util.ArrayList;
import java.util.List;
import java.util.Properties;
import org.komodo.modeshape.visitor.DdlNodeVisitor;
import org.komodo.relational.Messages.Relational;
import org.komodo.relational.RelationalProperties;
import org.komodo.relational.internal.AdapterFactory;
import org.komodo.relational.internal.RelationalModelFactory;
import org.komodo.relational.internal.RelationalObjectImpl;
import org.komodo.relational.internal.TypeResolver;
import org.komodo.relational.model.Function;
import org.komodo.relational.model.Model;
import org.komodo.relational.model.Procedure;
import org.komodo.relational.model.PushdownFunction;
import org.komodo.relational.model.SchemaElement.SchemaElementType;
import org.komodo.relational.model.StoredProcedure;
import org.komodo.relational.model.Table;
import org.komodo.relational.model.UserDefinedFunction;
import org.komodo.relational.model.View;
import org.komodo.relational.model.VirtualProcedure;
import org.komodo.relational.vdb.ModelSource;
import org.komodo.relational.vdb.Vdb;
import org.komodo.relational.vdb.internal.ModelSourceImpl;
import org.komodo.repository.ObjectImpl;
import org.komodo.spi.KException;
import org.komodo.spi.Messages;
import org.komodo.spi.repository.KomodoObject;
import org.komodo.spi.repository.KomodoType;
import org.komodo.spi.repository.Property;
import org.komodo.spi.repository.PropertyValueType;
import org.komodo.spi.repository.Repository;
import org.komodo.spi.repository.Repository.UnitOfWork;
import org.komodo.spi.repository.Repository.UnitOfWork.State;
import org.komodo.spi.runtime.version.TeiidVersionProvider;
import org.komodo.utils.ArgCheck;
import org.komodo.utils.StringUtils;
import org.modeshape.sequencer.ddl.dialect.teiid.TeiidDdlLexicon.CreateProcedure;
import org.modeshape.sequencer.ddl.dialect.teiid.TeiidDdlLexicon.CreateTable;
import org.modeshape.sequencer.ddl.dialect.teiid.TeiidDdlLexicon.SchemaElement;
import org.modeshape.sequencer.teiid.lexicon.CoreLexicon;
import org.modeshape.sequencer.teiid.lexicon.VdbLexicon;

/**
 * An implementation of a relational model.
 */
public final class ModelImpl extends RelationalObjectImpl implements Model {

    /**
     * The allowed child types.
     */
    private static final KomodoType[] CHILD_TYPES = new KomodoType[] { PushdownFunction.IDENTIFIER, ModelSource.IDENTIFIER,
                                                                      StoredProcedure.IDENTIFIER, Table.IDENTIFIER,
                                                                      UserDefinedFunction.IDENTIFIER, View.IDENTIFIER,
                                                                      VirtualProcedure.IDENTIFIER };

    /**
     * The resolver of a {@link Model}.
     */
    public static final TypeResolver< Model > RESOLVER = new TypeResolver< Model >() {

        /**
         * {@inheritDoc}
         *
         * @see org.komodo.relational.internal.TypeResolver#create(org.komodo.spi.repository.Repository.UnitOfWork,
         *      org.komodo.spi.repository.Repository, org.komodo.spi.repository.KomodoObject, java.lang.String,
         *      org.komodo.relational.RelationalProperties)
         */
        @Override
        public Model create( final UnitOfWork transaction,
                             final Repository repository,
                             final KomodoObject parent,
                             final String id,
                             final RelationalProperties properties ) throws KException {
            final AdapterFactory adapter = new AdapterFactory( repository );
            final Vdb parentVdb = adapter.adapt( transaction, parent, Vdb.class );

            if ( parentVdb == null ) {
                throw new KException( Messages.getString( Relational.INVALID_PARENT_TYPE,
                                                          parent.getAbsolutePath(),
                                                          Model.class.getSimpleName() ) );
            }

            return parentVdb.addModel( transaction, id );
        }

        /**
         * {@inheritDoc}
         *
         * @see org.komodo.relational.internal.TypeResolver#identifier()
         */
        @Override
        public KomodoType identifier() {
            return IDENTIFIER;
        }

        /**
         * {@inheritDoc}
         *
         * @see org.komodo.relational.internal.TypeResolver#owningClass()
         */
        @Override
        public Class< ModelImpl > owningClass() {
            return ModelImpl.class;
        }

        /**
         * {@inheritDoc}
         *
         * @see org.komodo.relational.internal.TypeResolver#resolvable(org.komodo.spi.repository.Repository.UnitOfWork,
         *      org.komodo.spi.repository.KomodoObject)
         */
        @Override
        public boolean resolvable( final UnitOfWork transaction,
                                   final KomodoObject kobject ) throws KException {
            return ObjectImpl.validateType( transaction, kobject.getRepository(), kobject, VdbLexicon.Vdb.DECLARATIVE_MODEL );
        }

        /**
         * {@inheritDoc}
         *
         * @see org.komodo.relational.internal.TypeResolver#resolve(org.komodo.spi.repository.Repository.UnitOfWork,
         *      org.komodo.spi.repository.KomodoObject)
         */
        @Override
        public Model resolve( final UnitOfWork transaction,
                              final KomodoObject kobject ) throws KException {
            return new ModelImpl( transaction, kobject.getRepository(), kobject.getAbsolutePath() );
        }

    };

    /**
     * @param uow
     *        the transaction (cannot be <code>null</code> or have a state that is not {@link State#NOT_STARTED})
     * @param repository
     *        the repository where the relational object exists (cannot be <code>null</code>)
     * @param workspacePath
     *        the workspace relative path (cannot be empty)
     * @throws KException
     *         if an error occurs or if node at specified path is not a model
     */
    public ModelImpl( final UnitOfWork uow,
                      final Repository repository,
                      final String workspacePath ) throws KException {
        super( uow, repository, workspacePath );
    }

    @Override
    public KomodoType getTypeIdentifier( UnitOfWork uow ) {
        return RESOLVER.identifier();
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.model.Model#addPushdownFunction(org.komodo.spi.repository.Repository.UnitOfWork,
     *      java.lang.String)
     */
    @Override
    public PushdownFunction addPushdownFunction( final UnitOfWork transaction,
                                                 final String functionName ) throws KException {
        return RelationalModelFactory.createPushdownFunction( transaction, getRepository(), this, functionName );
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.model.Model#addUserDefinedFunction(org.komodo.spi.repository.Repository.UnitOfWork,
     *      java.lang.String)
     */
    @Override
    public UserDefinedFunction addUserDefinedFunction( final UnitOfWork transaction,
                                                       final String functionName ) throws KException {
        return RelationalModelFactory.createUserDefinedFunction( transaction, getRepository(), this, functionName );
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.model.Model#addStoredProcedure(org.komodo.spi.repository.Repository.UnitOfWork,
     *      java.lang.String)
     */
    @Override
    public StoredProcedure addStoredProcedure( final UnitOfWork transaction,
                                               final String procedureName ) throws KException {
        return RelationalModelFactory.createStoredProcedure( transaction, getRepository(), this, procedureName );
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.model.Model#addVirtualProcedure(org.komodo.spi.repository.Repository.UnitOfWork,
     *      java.lang.String)
     */
    @Override
    public VirtualProcedure addVirtualProcedure( final UnitOfWork transaction,
                                                 final String procedureName ) throws KException {
        return RelationalModelFactory.createVirtualProcedure( transaction, getRepository(), this, procedureName );
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.model.Model#addSource(org.komodo.spi.repository.Repository.UnitOfWork, java.lang.String)
     */
    @Override
    public ModelSource addSource( final UnitOfWork transaction,
                                  final String sourceName ) throws KException {
        return RelationalModelFactory.createModelSource( transaction, getRepository(), this, sourceName );
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.model.Model#addTable(org.komodo.spi.repository.Repository.UnitOfWork, java.lang.String)
     */
    @Override
    public Table addTable( final UnitOfWork transaction,
                           final String tableName ) throws KException {
        return RelationalModelFactory.createTable( transaction, getRepository(), this, tableName );
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.model.Model#addView(org.komodo.spi.repository.Repository.UnitOfWork, java.lang.String)
     */
    @Override
    public View addView( final UnitOfWork transaction,
                         final String viewName ) throws KException {
        return RelationalModelFactory.createView( transaction, getRepository(), this, viewName );
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.internal.RelationalObjectImpl#getChildren(org.komodo.spi.repository.Repository.UnitOfWork)
     */
    @Override
    public KomodoObject[] getChildren( final UnitOfWork transaction ) throws KException {
        ArgCheck.isNotNull( transaction, "transaction" ); //$NON-NLS-1$
        ArgCheck.isTrue( ( transaction.getState() == State.NOT_STARTED ), "transaction state is not NOT_STARTED" ); //$NON-NLS-1$

        // sources are found under a grouping node
        KomodoObject[] kids = super.getChildren( transaction );
        KomodoObject[] result = null;

        if ( kids.length == 0 ) {
            result = kids;
        } else {
            final List< KomodoObject > temp = new ArrayList<>();
            boolean foundGroup = false;

            for ( final KomodoObject kid : kids ) {
                // found sources grouping node so remove grouping node and add in sources
                if ( VdbLexicon.Vdb.SOURCES.equals( kid.getName( transaction ) ) ) {
                    foundGroup = true;

                    for ( final KomodoObject source : getSources( transaction ) ) {
                        temp.add( source );
                    }
                } else {
                    temp.add( kid );
                }
            }

            if ( foundGroup ) {
                result = temp.toArray( new KomodoObject[ temp.size() ] );
            } else {
                result = kids;
            }
        }

        return result;
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.repository.ObjectImpl#getChildTypes()
     */
    @Override
    public KomodoType[] getChildTypes() {
        return CHILD_TYPES;
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.model.Model#getDescription(org.komodo.spi.repository.Repository.UnitOfWork)
     */
    @Override
    public String getDescription( final UnitOfWork transaction ) throws KException {
        return getObjectProperty( transaction, PropertyValueType.STRING, "getDescription", VdbLexicon.Vdb.DESCRIPTION ); //$NON-NLS-1$
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.model.Model#getFunctions(org.komodo.spi.repository.Repository.UnitOfWork)
     */
    @Override
    public Function[] getFunctions( final UnitOfWork transaction ) throws KException {
        ArgCheck.isNotNull( transaction, "transaction" ); //$NON-NLS-1$
        ArgCheck.isTrue( ( transaction.getState() == State.NOT_STARTED ), "transaction state is not NOT_STARTED" ); //$NON-NLS-1$

        final List< Function > result = new ArrayList< Function >();

        for ( final KomodoObject kobject : super.getChildrenOfType( transaction, CreateProcedure.FUNCTION_STATEMENT ) ) {
            final Property prop = kobject.getProperty( transaction, SchemaElement.TYPE );
            assert ( prop != null );

            final String value = prop.getStringValue( transaction );
            final SchemaElementType schemaType = SchemaElementType.fromValue( value );
            Function function = null;

            if ( schemaType == SchemaElementType.FOREIGN ) {
                function = new PushdownFunctionImpl( transaction, getRepository(), kobject.getAbsolutePath() );
            } else if ( schemaType == SchemaElementType.VIRTUAL ) {
                function = new UserDefinedFunctionImpl( transaction, getRepository(), kobject.getAbsolutePath() );
            }

            result.add( function );
        }

        if ( result.isEmpty() ) {
            return Function.NO_FUNCTIONS;
        }

        return result.toArray( new Function[ result.size() ] );
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.model.Model#getMetadataType(org.komodo.spi.repository.Repository.UnitOfWork)
     */
    @Override
    public String getMetadataType( final UnitOfWork transaction ) throws KException {
        final String result = getObjectProperty( transaction, PropertyValueType.STRING, "getMetadataType", //$NON-NLS-1$
                                                 VdbLexicon.Model.METADATA_TYPE );
        // if no metadata type return default value if there is a model definition
        if ( StringUtils.isBlank( result ) ) {
            return StringUtils.isBlank( getModelDefinition( transaction ) ) ? EMPTY_STRING : DEFAULT_METADATA_TYPE;
        }

        return result;
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.model.Model#getModelDefinition(org.komodo.spi.repository.Repository.UnitOfWork)
     */
    @Override
    public String getModelDefinition( final UnitOfWork uow ) throws KException {
        final String modelDefn = getObjectProperty( uow, PropertyValueType.STRING, "getModelDefinition", //$NON-NLS-1$
                                                    VdbLexicon.Model.MODEL_DEFINITION );
        return modelDefn == null ? EMPTY_STRING : modelDefn;
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.model.Model#getModelType(org.komodo.spi.repository.Repository.UnitOfWork)
     */
    @Override
    public Type getModelType( final UnitOfWork uow ) throws KException {
        final String value = getObjectProperty( uow, PropertyValueType.STRING, "getModelType", //$NON-NLS-1$
                                                CoreLexicon.JcrId.MODEL_TYPE );
        final Type modelType = ( ( value == null ) ? null : Type.valueOf( value ) );
        return ( ( modelType == null ) ? Type.DEFAULT_VALUE : modelType );
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.model.Model#getProcedures(org.komodo.spi.repository.Repository.UnitOfWork)
     */
    @Override
    public Procedure[] getProcedures( final UnitOfWork transaction ) throws KException {
        ArgCheck.isNotNull( transaction, "transaction" ); //$NON-NLS-1$
        ArgCheck.isTrue( ( transaction.getState() == State.NOT_STARTED ), "transaction state is not NOT_STARTED" ); //$NON-NLS-1$

        final List< Procedure > result = new ArrayList< Procedure >();

        for ( final KomodoObject kobject : super.getChildrenOfType( transaction, CreateProcedure.PROCEDURE_STATEMENT ) ) {
            final Property prop = kobject.getProperty( transaction, SchemaElement.TYPE );
            assert ( prop != null );

            final String value = prop.getStringValue( transaction );
            final SchemaElementType schemaType = SchemaElementType.fromValue( value );
            Procedure procedure = null;

            if ( schemaType == SchemaElementType.FOREIGN ) {
                procedure = new StoredProcedureImpl( transaction, getRepository(), kobject.getAbsolutePath() );
            } else {
                procedure = new VirtualProcedureImpl( transaction, getRepository(), kobject.getAbsolutePath() );
            }

            result.add( procedure );
        }

        if ( result.isEmpty() ) {
            return Procedure.NO_PROCEDURES;
        }

        return result.toArray( new Procedure[ result.size() ] );
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.model.Model#getSources(org.komodo.spi.repository.Repository.UnitOfWork)
     */
    @Override
    public ModelSource[] getSources( final UnitOfWork transaction ) throws KException {
        ArgCheck.isNotNull( transaction, "transaction" ); //$NON-NLS-1$
        ArgCheck.isTrue( ( transaction.getState() == State.NOT_STARTED ), "transaction state is not NOT_STARTED" ); //$NON-NLS-1$

        ModelSource[] result = null;

        if ( hasChild( transaction, VdbLexicon.Vdb.SOURCES, VdbLexicon.Vdb.SOURCES ) ) {
            final KomodoObject grouping = getChild( transaction, VdbLexicon.Vdb.SOURCES, VdbLexicon.Vdb.SOURCES );
            final List< ModelSource > temp = new ArrayList<>();

            for ( final KomodoObject kobject : grouping.getChildrenOfType( transaction, VdbLexicon.Source.SOURCE ) ) {
                final ModelSource translator = new ModelSourceImpl( transaction, getRepository(), kobject.getAbsolutePath() );
                temp.add( translator );
            }

            result = temp.toArray( new ModelSource[ temp.size() ] );
        } else {
            result = ModelSource.NO_SOURCES;
        }

        return result;
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.model.Model#getTables(org.komodo.spi.repository.Repository.UnitOfWork)
     */
    @Override
    public Table[] getTables( final UnitOfWork transaction ) throws KException {
        ArgCheck.isNotNull( transaction, "transaction" ); //$NON-NLS-1$
        ArgCheck.isTrue( ( transaction.getState() == State.NOT_STARTED ), "transaction state is not NOT_STARTED" ); //$NON-NLS-1$

        final List< Table > result = new ArrayList< Table >();

        for ( final KomodoObject kobject : super.getChildrenOfType( transaction, CreateTable.TABLE_STATEMENT ) ) {
            final Table table = new TableImpl( transaction, getRepository(), kobject.getAbsolutePath() );
            result.add( table );
        }

        if ( result.isEmpty() ) {
            return Table.NO_TABLES;
        }

        return result.toArray( new Table[ result.size() ] );
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.spi.repository.KomodoObject#getTypeId()
     */
    @Override
    public int getTypeId() {
        return TYPE_ID;
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.model.Model#getViews(org.komodo.spi.repository.Repository.UnitOfWork)
     */
    @Override
    public View[] getViews( final UnitOfWork transaction ) throws KException {
        ArgCheck.isNotNull( transaction, "transaction" ); //$NON-NLS-1$
        ArgCheck.isTrue( ( transaction.getState() == State.NOT_STARTED ), "transaction state is not NOT_STARTED" ); //$NON-NLS-1$

        final List< View > result = new ArrayList< View >();

        for ( final KomodoObject kobject : super.getChildrenOfType( transaction, CreateTable.VIEW_STATEMENT ) ) {
            final View view = new ViewImpl( transaction, getRepository(), kobject.getAbsolutePath() );
            result.add( view );
        }

        if ( result.isEmpty() ) {
            return View.NO_VIEWS;
        }

        return result.toArray( new View[ result.size() ] );
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.model.Model#isVisible(org.komodo.spi.repository.Repository.UnitOfWork)
     */
    @Override
    public boolean isVisible( final UnitOfWork transaction ) throws KException {
        final Boolean value = getObjectProperty( transaction, PropertyValueType.BOOLEAN, "isVisible", VdbLexicon.Model.VISIBLE ); //$NON-NLS-1$

        if ( value == null ) {
            return DEFAULT_VISIBLE;
        }

        return value;
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.model.Model#removeFunction(org.komodo.spi.repository.Repository.UnitOfWork, java.lang.String)
     */
    @Override
    public void removeFunction( final UnitOfWork transaction,
                                final String functionName ) throws KException {
        ArgCheck.isNotNull( transaction, "transaction" ); //$NON-NLS-1$
        ArgCheck.isTrue( ( transaction.getState() == State.NOT_STARTED ), "transaction state is not NOT_STARTED" ); //$NON-NLS-1$
        ArgCheck.isNotEmpty( functionName, "functionName" ); //$NON-NLS-1$

        boolean found = false;
        final Function[] functions = getFunctions( transaction );

        if ( functions.length != 0 ) {
            for ( final Function function : functions ) {
                if ( functionName.equals( function.getName( transaction ) ) ) {
                    function.remove( transaction );
                    found = true;
                    break;
                }
            }
        }

        if ( !found ) {
            throw new KException( Messages.getString( Relational.FUNCTION_NOT_FOUND_TO_REMOVE, functionName ) );
        }
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.model.Model#removeProcedure(org.komodo.spi.repository.Repository.UnitOfWork, java.lang.String)
     */
    @Override
    public void removeProcedure( final UnitOfWork transaction,
                                 final String procedureName ) throws KException {
        ArgCheck.isNotNull( transaction, "transaction" ); //$NON-NLS-1$
        ArgCheck.isTrue( ( transaction.getState() == State.NOT_STARTED ), "transaction state is not NOT_STARTED" ); //$NON-NLS-1$
        ArgCheck.isNotEmpty( procedureName, "procedureName" ); //$NON-NLS-1$

        boolean found = false;
        final Procedure[] procedures = getProcedures( transaction );

        if ( procedures.length != 0 ) {
            for ( final Procedure procedure : procedures ) {
                if ( procedureName.equals( procedure.getName( transaction ) ) ) {
                    procedure.remove( transaction );
                    found = true;
                    break;
                }
            }
        }

        if ( !found ) {
            throw new KException( Messages.getString( Relational.PROCEDURE_NOT_FOUND_TO_REMOVE, procedureName ) );
        }
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.model.Model#removeSource(org.komodo.spi.repository.Repository.UnitOfWork, java.lang.String)
     */
    @Override
    public void removeSource( final UnitOfWork transaction,
                              final String sourceToRemove ) throws KException {
        ArgCheck.isNotNull( transaction, "transaction" ); //$NON-NLS-1$
        ArgCheck.isTrue( ( transaction.getState() == State.NOT_STARTED ), "transaction state is not NOT_STARTED" ); //$NON-NLS-1$
        ArgCheck.isNotEmpty( sourceToRemove, "sourceToRemove" ); //$NON-NLS-1$

        boolean found = false;

        final ModelSource[] sources = getSources( transaction );

        if ( sources.length != 0 ) {
            for ( final ModelSource source : sources ) {
                if ( sourceToRemove.equals( source.getName( transaction ) ) ) {
                    source.remove( transaction );
                    found = true;
                    break;
                }
            }
        }

        if ( !found ) {
            throw new KException( Messages.getString( Relational.MODEL_SOURCE_NOT_FOUND_TO_REMOVE, sourceToRemove ) );
        }
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.model.Model#removeTable(org.komodo.spi.repository.Repository.UnitOfWork, java.lang.String)
     */
    @Override
    public void removeTable( final UnitOfWork transaction,
                             final String tableName ) throws KException {
        ArgCheck.isNotNull( transaction, "transaction" ); //$NON-NLS-1$
        ArgCheck.isTrue( ( transaction.getState() == State.NOT_STARTED ), "transaction state is not NOT_STARTED" ); //$NON-NLS-1$
        ArgCheck.isNotEmpty( tableName, "tableName" ); //$NON-NLS-1$

        boolean found = false;
        final Table[] tables = getTables( transaction );

        if ( tables.length != 0 ) {
            for ( final Table table : tables ) {
                if ( tableName.equals( table.getName( transaction ) ) ) {
                    table.remove( transaction );
                    found = true;
                    break;
                }
            }
        }

        if ( !found ) {
            throw new KException( Messages.getString( Relational.TABLE_NOT_FOUND_TO_REMOVE, tableName ) );
        }
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.model.Model#removeView(org.komodo.spi.repository.Repository.UnitOfWork, java.lang.String)
     */
    @Override
    public void removeView( final UnitOfWork transaction,
                            final String viewName ) throws KException {
        ArgCheck.isNotNull( transaction, "transaction" ); //$NON-NLS-1$
        ArgCheck.isTrue( ( transaction.getState() == State.NOT_STARTED ), "transaction state is not NOT_STARTED" ); //$NON-NLS-1$
        ArgCheck.isNotEmpty( viewName, "viewName" ); //$NON-NLS-1$

        boolean found = false;
        final View[] views = getViews( transaction );

        if ( views.length != 0 ) {
            for ( final View view : views ) {
                if ( viewName.equals( view.getName( transaction ) ) ) {
                    view.remove( transaction );
                    found = true;
                    break;
                }
            }
        }

        if ( !found ) {
            throw new KException( Messages.getString( Relational.VIEW_NOT_FOUND_TO_REMOVE, viewName ) );
        }
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.model.Model#setDescription(org.komodo.spi.repository.Repository.UnitOfWork, java.lang.String)
     */
    @Override
    public void setDescription( final UnitOfWork transaction,
                                final String newDescription ) throws KException {
        setObjectProperty( transaction, "setDescription", VdbLexicon.Vdb.DESCRIPTION, newDescription ); //$NON-NLS-1$
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.model.Model#setMetadataType(org.komodo.spi.repository.Repository.UnitOfWork, java.lang.String)
     */
    @Override
    public void setMetadataType( final UnitOfWork transaction,
                                 final String newMetadataType ) throws KException {
        String value = newMetadataType;

        if ( StringUtils.isBlank( newMetadataType ) ) {
            value = ( StringUtils.isBlank( getModelDefinition( transaction ) ) ? value : DEFAULT_METADATA_TYPE );
        }

        setObjectProperty( transaction, "setMetadataType", VdbLexicon.Model.METADATA_TYPE, value ); //$NON-NLS-1$
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.model.Model#setModelDefinition(org.komodo.spi.repository.Repository.UnitOfWork,
     *      java.lang.String)
     */
    @Override
    public void setModelDefinition( final UnitOfWork uow,
                                    final String modelDefinition ) throws KException {
        setObjectProperty( uow, "setModelDefinition", VdbLexicon.Model.MODEL_DEFINITION, modelDefinition ); //$NON-NLS-1$

        if ( StringUtils.isBlank( modelDefinition ) ) {
            // if no model definition make sure there isn't a metadata type
            if ( !StringUtils.isBlank( getMetadataType( uow ) ) ) {
                setMetadataType( uow, EMPTY_STRING );
            }
        } else {
            // if there is a model definition make sure there is a metadata type
            if ( StringUtils.isBlank( getMetadataType( uow ) ) ) {
                setMetadataType( uow, DEFAULT_METADATA_TYPE );
            }
        }
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.model.Model#setModelType(org.komodo.spi.repository.Repository.UnitOfWork,
     *      org.komodo.relational.model.Model.Type)
     */
    @Override
    public void setModelType( final UnitOfWork uow,
                              final Type newModelType ) throws KException {
        final Type modelType = ( ( newModelType == null ) ? Type.DEFAULT_VALUE : newModelType );
        setObjectProperty( uow, "setModelType", CoreLexicon.JcrId.MODEL_TYPE, modelType.name() ); //$NON-NLS-1$
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.model.Model#setVisible(org.komodo.spi.repository.Repository.UnitOfWork, boolean)
     */
    @Override
    public void setVisible( final UnitOfWork transaction,
                            final boolean newVisible ) throws KException {
        setObjectProperty( transaction, "setVisible", VdbLexicon.Model.VISIBLE, newVisible ); //$NON-NLS-1$
    }

    private String exportDdl(UnitOfWork transaction, Properties exportProperties) throws Exception {
        DdlNodeVisitor visitor = new DdlNodeVisitor(TeiidVersionProvider.getInstance().getTeiidVersion(), false);
        visitor.visit(node(transaction));

        String result = visitor.getDdl();
        return result;
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.spi.repository.Exportable#export(org.komodo.spi.repository.Repository.UnitOfWork, java.util.Properties)
     */
    @Override
    public String export( final UnitOfWork transaction , Properties exportProperties) throws KException {
        ArgCheck.isNotNull(transaction);

        if (LOGGER.isDebugEnabled()) {
            LOGGER.debug("modelimpl-export: transaction = {0}", transaction.getName()); //$NON-NLS-1$
        }

        try {
            String result = exportDdl(transaction, exportProperties);

            if (LOGGER.isDebugEnabled()) {
                LOGGER.debug("ModelImpl: transaction = {0}, xml = {1}", //$NON-NLS-1$
                             transaction.getName(),
                             result);
            }

            return result;

        } catch (final Exception e) {
            throw handleError(e);
        }
    }
}
