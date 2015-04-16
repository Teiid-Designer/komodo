/*
 * JBoss, Home of Professional Open Source.
 *
 * See the LEGAL.txt file distributed with this work for information regarding copyright ownership and licensing.
 *
 * See the AUTHORS.txt file distributed with this work for a full listing of individual contributors.
 */
package org.komodo.repository;

import java.util.List;
import javax.jcr.Node;
import javax.jcr.Session;
import org.komodo.core.KomodoLexicon;
import org.komodo.core.KomodoLexicon.Komodo;
import org.komodo.repository.RepositoryImpl.UnitOfWorkImpl;
import org.komodo.repository.internal.RuleImpl;
import org.komodo.spi.KException;
import org.komodo.spi.constants.StringConstants;
import org.komodo.spi.repository.EnvironmentStore;
import org.komodo.spi.repository.KomodoObject;
import org.komodo.spi.repository.Repository;
import org.komodo.spi.repository.Repository.UnitOfWork;
import org.komodo.spi.repository.validation.Rule;
import org.komodo.spi.repository.validation.Rule.MessageKey;
import org.komodo.spi.utils.LocalizedMessage;
import org.komodo.utils.ArgCheck;
import org.komodo.utils.KLog;
import org.modeshape.jcr.api.JcrTools;

/**
 * An implementation of an {@link EnvironmentStore} that can be used by repositories.
 */
public class EnvironmentStoreDelegate implements EnvironmentStore {

    /**
     * The root path of the Komodo repository library.
     */
    public static String ENV_ROOT = ( RepositoryImpl.KOMODO_ROOT + StringConstants.FORWARD_SLASH + Komodo.ENVIRONMENT );

    private static final KLog LOGGER = KLog.getLogger();

    /**
     * The root path of the Komodo environment validation area.
     */
    public static String VALIDATION_ROOT = ( ENV_ROOT + StringConstants.FORWARD_SLASH + KomodoLexicon.Environment.VALIDATION );

    private final Repository repo;

    /**
     * Constructs an environment store delegate.
     *
     * @param repo
     *        the repository this is a delegate for (cannot be <code>null</code>)
     */
    public EnvironmentStoreDelegate( final Repository repo ) {
        this.repo = repo;
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.spi.repository.EnvironmentStore#addChildCountValidationRule(org.komodo.spi.repository.Repository.UnitOfWork,
     *      java.lang.String, java.lang.String, java.lang.String, java.lang.Number, boolean, java.lang.Number, boolean,
     *      java.util.List, java.util.List)
     */
    @Override
    public Rule addChildCountValidationRule( final UnitOfWork uow,
                                             final String name,
                                             final String nodeType,
                                             final String childType,
                                             final Number minValue,
                                             final boolean minInclusive,
                                             final Number maxValue,
                                             final boolean maxInclusive,
                                             final List< LocalizedMessage > descriptions,
                                             final List< LocalizedMessage > messages ) throws KException {
        ArgCheck.isNotEmpty( childType, "childType" ); //$NON-NLS-1$
        ArgCheck.isTrue( ( minValue != null ) || ( maxValue != null ), "minValue or maxValue must not be null" ); //$NON-NLS-1$

        final UnitOfWork transaction = verifyTransaction( uow, "addChildCountValidationRule", false ); //$NON-NLS-1$

        if (LOGGER.isDebugEnabled()) {
            LOGGER.debug( "addChildCountValidationRule: transaction = {0}, name = {1}", transaction.getName(), name ); //$NON-NLS-1$
        }

        try {
            final RuleImpl rule = createRule( transaction,
                                              name,
                                              KomodoLexicon.Rule.NUMBER_RULE,
                                              Rule.ValidationType.CHILD,
                                              Rule.RuleType.NUMBER,
                                              nodeType,
                                              descriptions,
                                              messages );
            rule.setProperty( transaction, KomodoLexicon.Rule.JCR_NAME, childType );

            if (minValue != null) {
                rule.setProperty( transaction, KomodoLexicon.Rule.MIN_VALUE, minValue.toString() );
                rule.setProperty( transaction, KomodoLexicon.Rule.MIN_VALUE_INCLUSIVE, minInclusive );
            }

            if (maxValue != null) {
                rule.setProperty( transaction, KomodoLexicon.Rule.MAX_VALUE, maxValue.toString() );
                rule.setProperty( transaction, KomodoLexicon.Rule.MAX_VALUE_INCLUSIVE, maxInclusive );
            }

            if (uow == null) {
                transaction.commit();
            }

            return rule;
        } catch (final Exception e) {
            throw ObjectImpl.handleError( uow, transaction, e );
        }
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.spi.repository.EnvironmentStore#addChildRelationshipValidationRule(org.komodo.spi.repository.Repository.UnitOfWork,
     *      java.lang.String, java.lang.String, java.lang.String, java.util.List, java.util.List, java.util.List, java.util.List,
     *      java.util.List, java.util.List)
     */
    @Override
    public Rule addChildRelationshipValidationRule( final UnitOfWork uow,
                                                    final String name,
                                                    final String nodeType,
                                                    final String childType,
                                                    final List< String > propsThatMustExist,
                                                    final List< String > propsThatMustNotExist,
                                                    final List< String > childTypesThatMustExist,
                                                    final List< String > childTypesThatMustNotExist,
                                                    final List< LocalizedMessage > descriptions,
                                                    final List< LocalizedMessage > messages ) throws KException {
        ArgCheck.isNotEmpty( childType, "childType" ); //$NON-NLS-1$
        ArgCheck.isTrue( ( ( propsThatMustExist != null ) && !propsThatMustExist.isEmpty() )
                         || ( ( propsThatMustNotExist != null ) && !propsThatMustNotExist.isEmpty() )
                         || ( ( childTypesThatMustExist != null ) && !childTypesThatMustExist.isEmpty() )
                         || ( ( childTypesThatMustNotExist != null ) && !childTypesThatMustNotExist.isEmpty() ),
                         "at least one relationship collection must not be empty" ); //$NON-NLS-1$

        final UnitOfWork transaction = verifyTransaction( uow, "addChildRelationshipValidationRule", false ); //$NON-NLS-1$

        if (LOGGER.isDebugEnabled()) {
            LOGGER.debug( "addChildRelationshipValidationRule: transaction = {0}, name = {1}", transaction.getName(), name ); //$NON-NLS-1$
        }

        try {
            final RuleImpl rule = createRule( transaction,
                                              name,
                                              KomodoLexicon.Rule.RELATIONSHIP_RULE,
                                              Rule.ValidationType.CHILD,
                                              Rule.RuleType.RELATIONSHIP,
                                              nodeType,
                                              descriptions,
                                              messages );

            processMultiValuedProperty( transaction, rule, KomodoLexicon.Rule.PROP_EXISTS, propsThatMustExist );
            processMultiValuedProperty( transaction, rule, KomodoLexicon.Rule.PROP_ABSENT, propsThatMustNotExist );
            processMultiValuedProperty( transaction, rule, KomodoLexicon.Rule.CHILD_EXISTS, childTypesThatMustExist );
            processMultiValuedProperty( transaction, rule, KomodoLexicon.Rule.CHILD_ABSENT, childTypesThatMustNotExist );

            if (uow == null) {
                transaction.commit();
            }

            return rule;
        } catch (final Exception e) {
            throw ObjectImpl.handleError( uow, transaction, e );
        }
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.spi.repository.EnvironmentStore#addChildTypeRequiredRule(org.komodo.spi.repository.Repository.UnitOfWork,
     *      java.lang.String, java.lang.String, java.lang.String, java.util.List, java.util.List)
     */
    @Override
    public Rule addChildTypeRequiredRule( final UnitOfWork uow,
                                          final String name,
                                          final String nodeType,
                                          final String childType,
                                          final List< LocalizedMessage > descriptions,
                                          final List< LocalizedMessage > messages ) throws KException {
        ArgCheck.isNotEmpty( childType, "childType" ); //$NON-NLS-1$

        final UnitOfWork transaction = verifyTransaction( uow, "addChildTypeRequiredRule", false ); //$NON-NLS-1$

        if (LOGGER.isDebugEnabled()) {
            LOGGER.debug( "addChildTypeRequiredRule: transaction = {0}, name = {1}", transaction.getName(), name ); //$NON-NLS-1$
        }

        try {
            final RuleImpl rule = createRule( transaction,
                                              name,
                                              KomodoLexicon.Rule.REQUIRED_RULE,
                                              Rule.ValidationType.CHILD,
                                              Rule.RuleType.REQUIRED,
                                              nodeType,
                                              descriptions,
                                              messages );

            rule.setProperty( transaction, KomodoLexicon.Rule.JCR_NAME, childType );

            if (uow == null) {
                transaction.commit();
            }

            return rule;
        } catch (final Exception e) {
            throw ObjectImpl.handleError( uow, transaction, e );
        }
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.spi.repository.EnvironmentStore#addNodeNameRule(org.komodo.spi.repository.Repository.UnitOfWork,
     *      java.lang.String, java.lang.String, java.lang.String, java.util.List, java.util.List)
     */
    @Override
    public Rule addNodeNameRule( final UnitOfWork uow,
                                 final String name,
                                 final String nodeType,
                                 final String pattern,
                                 final List< LocalizedMessage > descriptions,
                                 final List< LocalizedMessage > messages ) throws KException {
        ArgCheck.isNotEmpty( pattern, "pattern" ); //$NON-NLS-1$

        final UnitOfWork transaction = verifyTransaction( uow, "addNodeNameRule", false ); //$NON-NLS-1$

        if (LOGGER.isDebugEnabled()) {
            LOGGER.debug( "addNodeNameRule: transaction = {0}, name = {1}", transaction.getName(), name ); //$NON-NLS-1$
        }

        try {
            final RuleImpl rule = createRule( transaction,
                                              name,
                                              KomodoLexicon.Rule.PATTERN_RULE,
                                              Rule.ValidationType.NODE,
                                              Rule.RuleType.PATTERN,
                                              nodeType,
                                              descriptions,
                                              messages );

            rule.setProperty( transaction, KomodoLexicon.Rule.PATTERN, pattern );

            if (uow == null) {
                transaction.commit();
            }

            return rule;
        } catch (final Exception e) {
            throw ObjectImpl.handleError( uow, transaction, e );
        }
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.spi.repository.EnvironmentStore#addPropertyPatternRule(org.komodo.spi.repository.Repository.UnitOfWork,
     *      java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.util.List, java.util.List)
     */
    @Override
    public Rule addPropertyPatternRule( final UnitOfWork uow,
                                        final String name,
                                        final String nodeType,
                                        final String propertyName,
                                        final String pattern,
                                        final List< LocalizedMessage > descriptions,
                                        final List< LocalizedMessage > messages ) throws KException {
        ArgCheck.isNotEmpty( propertyName, "propertyName" ); //$NON-NLS-1$
        ArgCheck.isNotEmpty( pattern, "pattern" ); //$NON-NLS-1$

        final UnitOfWork transaction = verifyTransaction( uow, "addPropertyPatternRule", false ); //$NON-NLS-1$

        if (LOGGER.isDebugEnabled()) {
            LOGGER.debug( "addPropertyPatternRule: transaction = {0}, name = {1}", transaction.getName(), name ); //$NON-NLS-1$
        }

        try {
            final RuleImpl rule = createRule( transaction,
                                              name,
                                              KomodoLexicon.Rule.PATTERN_RULE,
                                              Rule.ValidationType.PROPERTY,
                                              Rule.RuleType.PATTERN,
                                              nodeType,
                                              descriptions,
                                              messages );

            rule.setProperty( transaction, KomodoLexicon.Rule.JCR_NAME, propertyName );
            rule.setProperty( transaction, KomodoLexicon.Rule.PATTERN, pattern );

            if (uow == null) {
                transaction.commit();
            }

            return rule;
        } catch (final Exception e) {
            throw ObjectImpl.handleError( uow, transaction, e );
        }
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.spi.repository.EnvironmentStore#addPropertyRelationshipValidationRule(org.komodo.spi.repository.Repository.UnitOfWork,
     *      java.lang.String, java.lang.String, java.lang.String, java.util.List, java.util.List, java.util.List, java.util.List,
     *      java.util.List, java.util.List)
     */
    @Override
    public Rule addPropertyRelationshipValidationRule( final UnitOfWork uow,
                                                       final String name,
                                                       final String nodeType,
                                                       final String propertyName,
                                                       final List< String > propsThatMustExist,
                                                       final List< String > propsThatMustNotExist,
                                                       final List< String > childTypesThatMustExist,
                                                       final List< String > childTypesThatMustNotExist,
                                                       final List< LocalizedMessage > descriptions,
                                                       final List< LocalizedMessage > messages ) throws KException {
        ArgCheck.isNotEmpty( propertyName, "propertyName" ); //$NON-NLS-1$
        ArgCheck.isTrue( ( ( propsThatMustExist != null ) && !propsThatMustExist.isEmpty() )
                         || ( ( propsThatMustNotExist != null ) && !propsThatMustNotExist.isEmpty() )
                         || ( ( childTypesThatMustExist != null ) && !childTypesThatMustExist.isEmpty() )
                         || ( ( childTypesThatMustNotExist != null ) && !childTypesThatMustNotExist.isEmpty() ),
                         "at least one relationship collection must not be empty" ); //$NON-NLS-1$

        final UnitOfWork transaction = verifyTransaction( uow, "addPropertyRelationshipValidationRule", false ); //$NON-NLS-1$

        if (LOGGER.isDebugEnabled()) {
            LOGGER.debug( "addPropertyRelationshipValidationRule: transaction = {0}, name = {1}", transaction.getName(), name ); //$NON-NLS-1$
        }

        try {
            final RuleImpl rule = createRule( transaction,
                                              name,
                                              KomodoLexicon.Rule.RELATIONSHIP_RULE,
                                              Rule.ValidationType.PROPERTY,
                                              Rule.RuleType.RELATIONSHIP,
                                              nodeType,
                                              descriptions,
                                              messages );
            rule.setProperty( transaction, KomodoLexicon.Rule.JCR_NAME, propertyName );
            processMultiValuedProperty( transaction, rule, KomodoLexicon.Rule.PROP_EXISTS, propsThatMustExist );
            processMultiValuedProperty( transaction, rule, KomodoLexicon.Rule.PROP_ABSENT, propsThatMustNotExist );
            processMultiValuedProperty( transaction, rule, KomodoLexicon.Rule.CHILD_EXISTS, childTypesThatMustExist );
            processMultiValuedProperty( transaction, rule, KomodoLexicon.Rule.CHILD_ABSENT, childTypesThatMustNotExist );

            if (uow == null) {
                transaction.commit();
            }

            return rule;
        } catch (final Exception e) {
            throw ObjectImpl.handleError( uow, transaction, e );
        }
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.spi.repository.EnvironmentStore#addPropertyRequiredRule(org.komodo.spi.repository.Repository.UnitOfWork,
     *      java.lang.String, java.lang.String, java.lang.String, java.util.List, java.util.List)
     */
    @Override
    public Rule addPropertyRequiredRule( final UnitOfWork uow,
                                         final String name,
                                         final String nodeType,
                                         final String propertyName,
                                         final List< LocalizedMessage > descriptions,
                                         final List< LocalizedMessage > messages ) throws KException {
        ArgCheck.isNotEmpty( propertyName, "propertyName" ); //$NON-NLS-1$

        final UnitOfWork transaction = verifyTransaction( uow, "addPropertyRequiredRule", false ); //$NON-NLS-1$

        if (LOGGER.isDebugEnabled()) {
            LOGGER.debug( "addPropertyRequiredRule: transaction = {0}, name = {1}", transaction.getName(), name ); //$NON-NLS-1$
        }

        try {
            final RuleImpl rule = createRule( transaction,
                                              name,
                                              KomodoLexicon.Rule.REQUIRED_RULE,
                                              Rule.ValidationType.PROPERTY,
                                              Rule.RuleType.REQUIRED,
                                              nodeType,
                                              descriptions,
                                              messages );

            rule.setProperty( transaction, KomodoLexicon.Rule.JCR_NAME, propertyName );

            if (uow == null) {
                transaction.commit();
            }

            return rule;
        } catch (final Exception e) {
            throw ObjectImpl.handleError( uow, transaction, e );
        }
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.spi.repository.EnvironmentStore#addPropertyValueNumberValidationRule(org.komodo.spi.repository.Repository.UnitOfWork,
     *      java.lang.String, java.lang.String, java.lang.String, java.lang.Number, boolean, java.lang.Number, boolean,
     *      java.util.List, java.util.List)
     */
    @Override
    public Rule addPropertyValueNumberValidationRule( final UnitOfWork uow,
                                                      final String name,
                                                      final String nodeType,
                                                      final String propertyName,
                                                      final Number minValue,
                                                      final boolean minInclusive,
                                                      final Number maxValue,
                                                      final boolean maxInclusive,
                                                      final List< LocalizedMessage > descriptions,
                                                      final List< LocalizedMessage > messages ) throws KException {
        ArgCheck.isNotEmpty( propertyName, "propertyName" ); //$NON-NLS-1$
        ArgCheck.isTrue( ( minValue != null ) || ( maxValue != null ), "minValue or maxValue must not be null" ); //$NON-NLS-1$

        final UnitOfWork transaction = verifyTransaction( uow, "addPropertyValueNumberValidationRule", false ); //$NON-NLS-1$

        if (LOGGER.isDebugEnabled()) {
            LOGGER.debug( "addPropertyValueNumberValidationRule: transaction = {0}, name = {1}", transaction.getName(), name ); //$NON-NLS-1$
        }

        try {
            final RuleImpl rule = createRule( transaction,
                                              name,
                                              KomodoLexicon.Rule.NUMBER_RULE,
                                              Rule.ValidationType.PROPERTY,
                                              Rule.RuleType.NUMBER,
                                              nodeType,
                                              descriptions,
                                              messages );
            rule.setProperty( transaction, KomodoLexicon.Rule.JCR_NAME, propertyName );

            if (minValue != null) {
                rule.setProperty( transaction, KomodoLexicon.Rule.MIN_VALUE, minValue.toString() );
                rule.setProperty( transaction, KomodoLexicon.Rule.MIN_VALUE_INCLUSIVE, minInclusive );
            }

            if (maxValue != null) {
                rule.setProperty( transaction, KomodoLexicon.Rule.MAX_VALUE, maxValue.toString() );
                rule.setProperty( transaction, KomodoLexicon.Rule.MAX_VALUE_INCLUSIVE, maxInclusive );
            }

            if (uow == null) {
                transaction.commit();
            }

            return rule;
        } catch (final Exception e) {
            throw ObjectImpl.handleError( uow, transaction, e );
        }
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.spi.repository.EnvironmentStore#addSameNameSiblingValidationRule(org.komodo.spi.repository.Repository.UnitOfWork,
     *      java.lang.String, java.lang.String, boolean, java.util.List, java.util.List)
     */
    @Override
    public Rule addSameNameSiblingValidationRule( final UnitOfWork uow,
                                                  final String name,
                                                  final String nodeType,
                                                  final boolean matchType,
                                                  final List< LocalizedMessage > descriptions,
                                                  final List< LocalizedMessage > messages ) throws KException {
        final UnitOfWork transaction = verifyTransaction( uow, "addSameNameSiblingValidationRule", false ); //$NON-NLS-1$

        if (LOGGER.isDebugEnabled()) {
            LOGGER.debug( "addSameNameSiblingValidationRule: transaction = {0}, name = {1}", transaction.getName(), name ); //$NON-NLS-1$
        }

        try {
            final RuleImpl rule = createRule( transaction,
                                              name,
                                              KomodoLexicon.Rule.SNS_RULE,
                                              Rule.ValidationType.CHILD,
                                              Rule.RuleType.SAME_NAME_SIBLING,
                                              nodeType,
                                              descriptions,
                                              messages );

            rule.setProperty( transaction, KomodoLexicon.Rule.MATCH_TYPE, matchType );

            if (uow == null) {
                transaction.commit();
            }

            return rule;
        } catch (final Exception e) {
            throw ObjectImpl.handleError( uow, transaction, e );
        }
    }

    private RuleImpl createRule( final UnitOfWork uow,
                                 final String name,
                                 final String nodeType,
                                 final Rule.ValidationType validationType,
                                 final Rule.RuleType ruleType,
                                 final String ruleNodeType,
                                 final List< LocalizedMessage > descriptions,
                                 final List< LocalizedMessage > messages ) throws Exception {
        assert ( uow != null );
        assert ( validationType != null );
        assert ( ruleType != null );
        ArgCheck.isNotEmpty( name, "name" ); //$NON-NLS-1$
        ArgCheck.isNotEmpty( nodeType, "nodeType" ); //$NON-NLS-1$
        ArgCheck.isNotEmpty( ruleNodeType, "ruleNodeType" ); //$NON-NLS-1$
        ArgCheck.isNotEmpty( descriptions, "descriptions" ); //$NON-NLS-1$

        final KomodoObject parent = getValidationAreaNode( uow );
        final KomodoObject rule = parent.addChild( uow, name, nodeType );
        rule.setProperty( uow, KomodoLexicon.Rule.NODE_TYPE, ruleNodeType );
        rule.setProperty( uow, KomodoLexicon.Rule.VALIDATION_TYPE, validationType.name() );

        // add description and optional messages
        final KomodoObject messagesNode = rule.addChild( uow,
                                                         KomodoLexicon.Rule.MESSAGES,
                                                         KomodoLexicon.Rule.LOCALIZED_MESSAGE_GROUPING );

        { // add descriptions
            final KomodoObject description = messagesNode.addChild( uow,
                                                                    MessageKey.DESCRIPTION.name(),
                                                                    KomodoLexicon.Rule.LOCALIZED_MESSAGE );

            for (final LocalizedMessage localizedDescription : descriptions) {
                final KomodoObject node = description.addChild( uow,
                                                                localizedDescription.getLocaleCode(),
                                                                KomodoLexicon.Rule.LOCALIZED_TEXT_TYPE );
                node.setProperty( uow, KomodoLexicon.Rule.LOCALIZED_TEXT, localizedDescription.getMessage() );
            }
        }

        { // add messages
            if (( messages != null ) && !messages.isEmpty()) {
                for (final LocalizedMessage localizedMessage : messages) {
                    final String id = localizedMessage.getId();
                    KomodoObject message = null;

                    if (messagesNode.hasChild( uow, id )) {
                        message = messagesNode.getChild( uow, id );
                    } else {
                        message = messagesNode.addChild( uow, id, KomodoLexicon.Rule.LOCALIZED_MESSAGE );
                    }

                    final KomodoObject node = message.addChild( uow,
                                                                localizedMessage.getLocaleCode(),
                                                                KomodoLexicon.Rule.LOCALIZED_TEXT_TYPE );
                    node.setProperty( uow, KomodoLexicon.Rule.LOCALIZED_TEXT, localizedMessage.getMessage() );
                }
            }
        }

        return new RuleImpl( uow, this.repo, rule.getAbsolutePath() );
    }

    protected Session getSession( final UnitOfWork transaction ) {
        return ( ( UnitOfWorkImpl )transaction ).getSession();
    }

    private KomodoObject getValidationAreaNode( final UnitOfWork transaction ) throws Exception {
        assert ( transaction != null );

        final Session session = getSession( transaction );
        final JcrTools jcrTools = new JcrTools();

        jcrTools.findOrCreateNode( session, RepositoryImpl.KOMODO_ROOT );
        jcrTools.findOrCreateNode( session, ENV_ROOT );

        final Node node = jcrTools.findOrCreateNode( session, VALIDATION_ROOT );
        return new ObjectImpl( this.repo, node.getPath(), node.getIndex() );
    }

    private void processMultiValuedProperty( final UnitOfWork uow,
                                             final KomodoObject rule,
                                             final String propName,
                                             final List< String > values ) throws Exception {
        if (( values != null ) && !values.isEmpty()) {
            final String[] result = new String[ values.size() ];
            int i = 0;

            for (final String value : values) {
                ArgCheck.isNotEmpty( value, "value" ); //$NON-NLS-1$
                result[i++] = value;
            }

            rule.setProperty( uow, propName, ( Object[] )result );
        }
    }

    private UnitOfWork verifyTransaction( final UnitOfWork uow,
                                          final String name,
                                          final boolean rollback ) throws KException {
        UnitOfWork transaction = uow;

        if (transaction == null) {
            transaction = this.repo.createTransaction( EnvironmentStoreDelegate.class.getSimpleName() + '.' + name,
                                                       rollback,
                                                       null );
        }

        assert ( transaction != null );
        return transaction;
    }

}
