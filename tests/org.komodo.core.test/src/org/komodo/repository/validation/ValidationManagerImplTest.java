/*
 * JBoss, Home of Professional Open Source.
 *
 * See the LEGAL.txt file distributed with this work for information regarding copyright ownership and licensing.
 *
 * See the AUTHORS.txt file distributed with this work for a full listing of individual contributors.
 */
package org.komodo.repository.validation;

import static org.hamcrest.core.Is.is;
import static org.junit.Assert.assertThat;
import java.io.File;
import java.util.Arrays;
import java.util.Collections;
import java.util.List;
import org.junit.BeforeClass;
import org.junit.Test;
import org.junit.rules.TestName;
import org.komodo.spi.KException;
import org.komodo.spi.outcome.Outcome.Level;
import org.komodo.spi.repository.KomodoObject;
import org.komodo.spi.repository.Repository.UnitOfWork;
import org.komodo.spi.repository.ValidationManager;
import org.komodo.spi.repository.validation.Result;
import org.komodo.spi.repository.validation.Rule;
import org.komodo.spi.repository.validation.Rule.MessageKey;
import org.komodo.spi.utils.LocalizedMessage;
import org.komodo.test.utils.AbstractLocalRepositoryTest;

@SuppressWarnings( { "javadoc", "nls" } )
public final class ValidationManagerImplTest extends AbstractLocalRepositoryTest {

    private static List< LocalizedMessage > _description;
    private static List< LocalizedMessage > _message;
    private static ValidationManager _validationMgr;

    @BeforeClass
    public static void oneTimeSetup() throws Exception {
        _validationMgr = new ValidationManagerImpl( _repo );
        _description = Collections.singletonList( new LocalizedMessage( MessageKey.DESCRIPTION.name(),
                                                                        "en",
        "This is a rule description" ) );
        _message = Collections.singletonList( new LocalizedMessage( "myrule.msg", "en", "This is a rule error message" ) );
    }

    @org.junit.Rule
    public TestName name = new TestName();

    @Test( expected = IllegalArgumentException.class )
    public void shouldFailWhenNullRulesFile() throws Exception {
        _validationMgr.importRules( null );
    }

    @Test
    public void shouldFindErrorsInXmlWithErrors() throws Exception {
        final int numErrors = 12;
        final File testFile = new File( "resources/verifyValidationErrors.xml" );
        final List< String > errors = _validationMgr.importRules( testFile );
        assertThat( errors.size(), is( numErrors ) );

        // verify we got the errors we expect and the order we expect
        assertThat( errors.get( 0 ).contains( "Value 'BAD' is not facet-valid with respect to enumeration" ), is( true ) );
        assertThat( errors.get( 1 ).contains( "The value 'BAD' of attribute 'severity' on element 'nameValidation' is not valid" ),
                    is( true ) );
        assertThat( errors.get( 2 ).contains( "Value 'a' is not facet-valid with respect to pattern" ), is( true ) );
        assertThat( errors.get( 3 ).contains( "Element 'minValue' must have no element [children], and the value must be valid" ),
                    is( true ) );
        assertThat( errors.get( 4 ).contains( "Invalid content was found starting with element 'minValue'" ), is( true ) );
        assertThat( errors.get( 5 ).contains( "The content of element 'propertyValidation' is not complete" ), is( true ) );
        assertThat( errors.get( 6 ).contains( "Invalid content was found starting with element 'valueValidation'" ), is( true ) );
        assertThat( errors.get( 7 ).contains( "The content of element 'nodeValidation' is not complete" ), is( true ) );
        assertThat( errors.get( 8 ).contains( "Invalid content was found starting with element 'requiredValidation'" ), is( true ) );
        assertThat( errors.get( 9 ).contains( "The content of element 'valueRangeValidation' is not complete" ), is( true ) );
        assertThat( errors.get( 10 ).contains( "Value 'BAD_KEY' is not facet-valid with respect to enumeration" ), is( true ) );
        assertThat( errors.get( 11 ).contains( "The value 'BAD_KEY' of attribute 'key' on element 'message' is not valid with respect to its type, 'messageKeyType'" ),
                    is( true ) );
    }

    @Test
    public void shouldFindErrorsInXmlWithUniquenessProblems() throws Exception {
        final int numErrors = 16;
        final File testFile = new File( "resources/verifyUniquenessChecks.xml" );
        final List< String > errors = _validationMgr.importRules( testFile );
        assertThat( errors.size(), is( numErrors ) );

        // verify we got the errors we expect and the order we expect
        assertThat( errors.get( 0 ).contains( "Duplicate unique value [en]" ), is( true ) );
        assertThat( errors.get( 1 ).contains( "Duplicate unique value [es]" ), is( true ) );
        assertThat( errors.get( 2 ).contains( "Duplicate unique value [nz]" ), is( true ) );
        assertThat( errors.get( 3 ).contains( "Duplicate unique value [vdb:connectionType]" ), is( true ) );
        assertThat( errors.get( 4 ).contains( "Duplicate unique value [ca]" ), is( true ) );
        assertThat( errors.get( 5 ).contains( "Duplicate unique value [ddl:statementOption]" ), is( true ) );
        assertThat( errors.get( 6 ).contains( "Duplicate unique value [vdb:virtualDatabase]" ), is( true ) );
        assertThat( errors.get( 7 ).contains( "Duplicate unique value [ddl:statementOption.value]" ), is( true ) );
        assertThat( errors.get( 8 ).contains( "Duplicate unique value [ab] declared for identity constraint of element \"propertyValidation\"" ),
                    is( true ) );
        assertThat( errors.get( 9 ).contains( "Duplicate unique value [cd] declared for identity constraint of element \"propertyValidation\"" ),
                    is( true ) );
        assertThat( errors.get( 10 ).contains( "Duplicate unique value [ef] declared for identity constraint of element \"propertyValidation\"" ),
                    is( true ) );
        assertThat( errors.get( 11 ).contains( "Duplicate unique value [gh] declared for identity constraint of element \"propertyValidation\"" ),
                    is( true ) );
        assertThat( errors.get( 12 ).contains( "Duplicate unique value [REQUIRED_PROPERTY_NOT_FOUND,ij] declared for identity constraint of element \"propertyValidation\"" ),
                    is( true ) );
        assertThat( errors.get( 13 ).contains( "Duplicate unique value [PATTERN_RULE_INVALID_PROPERTY_VALUE,kl] declared for identity constraint of element \"propertyValidation\"" ),
                    is( true ) );
        assertThat( errors.get( 14 ).contains( "Duplicate unique value [PROPERTY_RULE_REQUIRED_PROPERTY_NOT_FOUND,mn] declared for identity constraint of element \"propertyValidation\"" ),
                    is( true ) );
        assertThat( errors.get( 15 ).contains( "Duplicate unique value [PROPERTY_RULE_VALUE_ABOVE_MAX_VALUE,op] declared for identity constraint of element \"propertyValidation\"" ),
                    is( true ) );
    }

    @Test
    public void shouldParseXmlWithNoErrors() throws Exception {
        final File testFile = new File( "resources/validationAllConstructs.xml" );
        final List< String > errors = _validationMgr.importRules( testFile );
        assertThat( errors.isEmpty(), is( true ) );
    }

    @Test
    public void shouldVerifyChildCountRuleFailsWhenCountEqualsMaxButMaxNotInclusive() throws Exception {
        final String childType = "nt:unstructured";
        final UnitOfWork uow = _repo.createTransaction( this.name.getMethodName(), false, null );

        // create rule in repo
        final Number minValue = 1;
        final Number maxValue = 3;
        final Rule rule = _validationMgr.addChildCountValidationRule( uow,
                                                                      "propRule",
                                                                      "nt:unstructured",
                                                                      childType,
                                                                      minValue,
                                                                      true,
                                                                      maxValue,
                                                                      false,
                                                                      _description,
                                                                      _message );

        // setup KomodoObject
        final KomodoObject kobject = _repo.add( uow, null, "kobject", null );

        for (int i = -1; i < maxValue.intValue(); ++i) {
            kobject.addChild( uow, ( "child" + i ), null );
        }

        uow.commit();

        // test
        final Result result = rule.evaluate( null, kobject );
        assertThat( result.getLevel(), is( Level.ERROR ) );
        assertThat( result.getPath(), is( kobject.getAbsolutePath() ) );
        assertThat( result.getRuleId(), is( rule.getName() ) );
    }

    @Test
    public void shouldVerifyChildCountRuleFailsWhenCountEqualsMinButMinNotInclusive() throws Exception {
        final String childType = "nt:unstructured";
        final UnitOfWork uow = _repo.createTransaction( this.name.getMethodName(), false, null );

        // create rule in repo
        final Number minValue = 1;
        final Number maxValue = 3;
        final Rule rule = _validationMgr.addChildCountValidationRule( uow,
                                                                      "propRule",
                                                                      "nt:unstructured",
                                                                      childType,
                                                                      minValue,
                                                                      false,
                                                                      maxValue,
                                                                      true,
                                                                      _description,
                                                                      _message );

        // setup KomodoObject
        final KomodoObject kobject = _repo.add( uow, null, "kobject", null );

        for (int i = 0; i < minValue.intValue(); ++i) {
            kobject.addChild( uow, ( "child" + i ), null );
        }

        uow.commit();

        // test
        final Result result = rule.evaluate( null, kobject );
        assertThat( result.getLevel(), is( Level.ERROR ) );
        assertThat( result.getPath(), is( kobject.getAbsolutePath() ) );
        assertThat( result.getRuleId(), is( rule.getName() ) );
    }

    @Test
    public void shouldVerifyChildCountRuleFailsWhenCountIsNotWithinRange() throws Exception {
        final String childType = "nt:unstructured";
        final UnitOfWork uow = _repo.createTransaction( this.name.getMethodName(), false, null );

        // create rule in repo
        final Number minValue = 1;
        final Number maxValue = 3;
        final Rule rule = _validationMgr.addChildCountValidationRule( uow,
                                                                      "propRule",
                                                                      "nt:unstructured",
                                                                      childType,
                                                                      minValue,
                                                                      true,
                                                                      maxValue,
                                                                      true,
                                                                      _description,
                                                                      _message );

        // setup KomodoObject
        final KomodoObject kobject = _repo.add( uow, null, "kobject", null );

        for (int i = -1; i < ( maxValue.intValue() + 1 ); ++i) {
            kobject.addChild( uow, ( "child" + i ), null );
        }

        uow.commit();

        // test
        final Result result = rule.evaluate( null, kobject );
        assertThat( result.getLevel(), is( Level.ERROR ) );
        assertThat( result.getPath(), is( kobject.getAbsolutePath() ) );
        assertThat( result.getRuleId(), is( rule.getName() ) );
    }

    @Test
    public void shouldVerifyChildCountRulePassesWhenCountWithinRange() throws Exception {
        final String childType = "nt:unstructured";
        final UnitOfWork uow = _repo.createTransaction( this.name.getMethodName(), false, null );

        // create rule in repo
        final Number minValue = 1;
        final Number maxValue = 3;
        final Rule rule = _validationMgr.addChildCountValidationRule( uow,
                                                                      "propRule",
                                                                      "nt:unstructured",
                                                                      childType,
                                                                      minValue,
                                                                      true,
                                                                      maxValue,
                                                                      true,
                                                                      _description,
                                                                      _message );

        // setup KomodoObject
        final KomodoObject kobject = _repo.add( uow, null, "kobject", null );

        for (int i = 0; i < maxValue.intValue(); ++i) {
            kobject.addChild( uow, ( "child" + i ), null );
        }

        uow.commit();

        // test
        final Result result = rule.evaluate( null, kobject );
        assertThat( result.isOK(), is( true ) );
        assertThat( result.getPath(), is( kobject.getAbsolutePath() ) );
        assertThat( result.getRuleId(), is( rule.getName() ) );
    }

    @Test( expected = KException.class )
    public void shouldVerifyDisabledRulesCannotBeEvaluated() throws Exception {
        final String propName = "sledge";
        final UnitOfWork uow = _repo.createTransaction( this.name.getMethodName(), false, null );

        // create rule in repo
        final Rule rule = _validationMgr.addPropertyRequiredRule( uow,
                                                                  "propRule",
                                                                  "nt:unstructured",
                                                                  propName,
                                                                  _description,
                                                                  _message );

        // setup KomodoObject
        final KomodoObject kobject = _repo.add( uow, null, "kobject", null );
        uow.commit();

        // disable rule
        rule.setEnabled( false );

        // test
        rule.evaluate( null, kobject );
    }

    @Test
    public void shouldVerifyNodeNameRuleFailsWhenNameDoesNotMatchesPattern() throws Exception {
        final UnitOfWork uow = _repo.createTransaction( this.name.getMethodName(), false, null );

        // create rule in repo
        final String pattern = "[A-Z]{3}";
        final Rule rule = _validationMgr.addNodeNameRule( uow, "propRule", "nt:unstructured", pattern, _description, _message );

        // setup KomodoObject
        final KomodoObject kobject = _repo.add( uow, null, "ABc", null );
        uow.commit();

        // test
        final Result result = rule.evaluate( null, kobject );
        assertThat( result.getLevel(), is( Level.ERROR ) );
        assertThat( result.getPath(), is( kobject.getAbsolutePath() ) );
        assertThat( result.getRuleId(), is( rule.getName() ) );
    }

    @Test
    public void shouldVerifyNodeNameRulePassesWhenNameMatchesPattern() throws Exception {
        final UnitOfWork uow = _repo.createTransaction( this.name.getMethodName(), false, null );

        // create rule in repo
        final String pattern = "[A-Z]{3}";
        final Rule rule = _validationMgr.addNodeNameRule( uow, "propRule", "nt:unstructured", pattern, _description, _message );

        // setup KomodoObject
        final KomodoObject kobject = _repo.add( uow, null, "ABC", null );
        uow.commit();

        // test
        final Result result = rule.evaluate( null, kobject );
        assertThat( result.isOK(), is( true ) );
        assertThat( result.getPath(), is( kobject.getAbsolutePath() ) );
        assertThat( result.getRuleId(), is( rule.getName() ) );
    }

    @Test
    public void shouldVerifyPropertyRangeRuleFailsWhenValueEqualsMaxButMaxNotInclusive() throws Exception {
        final String propName = "sledge";
        final UnitOfWork uow = _repo.createTransaction( this.name.getMethodName(), false, null );

        // create rule in repo
        final Number minValue = 5;
        final Number maxValue = 10;
        final Rule rule = _validationMgr.addPropertyValueNumberValidationRule( uow,
                                                                               "propRule",
                                                                               "nt:unstructured",
                                                                               propName,
                                                                               minValue,
                                                                               true,
                                                                               maxValue,
                                                                               false,
                                                                               _description,
                                                                               _message );

        // setup KomodoObject
        final KomodoObject kobject = _repo.add( uow, null, "kobject", null );
        kobject.setProperty( uow, propName, maxValue );
        uow.commit();

        // test
        final Result result = rule.evaluate( null, kobject );
        assertThat( result.getLevel(), is( Level.ERROR ) );
        assertThat( result.getPath(), is( kobject.getAbsolutePath() ) );
        assertThat( result.getRuleId(), is( rule.getName() ) );
    }

    @Test
    public void shouldVerifyPropertyRangeRuleFailsWhenValueEqualsMinButMinNotInclusive() throws Exception {
        final String propName = "sledge";
        final UnitOfWork uow = _repo.createTransaction( this.name.getMethodName(), false, null );

        // create rule in repo
        final Number minValue = 5;
        final Number maxValue = 10;
        final Rule rule = _validationMgr.addPropertyValueNumberValidationRule( uow,
                                                                               "propRule",
                                                                               "nt:unstructured",
                                                                               propName,
                                                                               minValue,
                                                                               false,
                                                                               maxValue,
                                                                               true,
                                                                               _description,
                                                                               _message );

        // setup KomodoObject
        final KomodoObject kobject = _repo.add( uow, null, "kobject", null );
        kobject.setProperty( uow, propName, minValue );
        uow.commit();

        // test
        final Result result = rule.evaluate( null, kobject );
        assertThat( result.getLevel(), is( Level.ERROR ) );
        assertThat( result.getPath(), is( kobject.getAbsolutePath() ) );
        assertThat( result.getRuleId(), is( rule.getName() ) );
    }

    @Test
    public void shouldVerifyPropertyRangeRulePassesWhenValueWithinRange() throws Exception {
        final String propName = "sledge";
        final UnitOfWork uow = _repo.createTransaction( this.name.getMethodName(), false, null );

        // create rule in repo
        final Number minValue = 5;
        final Number maxValue = 10;
        final Rule rule = _validationMgr.addPropertyValueNumberValidationRule( uow,
                                                                               "propRule",
                                                                               "nt:unstructured",
                                                                               propName,
                                                                               minValue,
                                                                               true,
                                                                               maxValue,
                                                                               true,
                                                                               _description,
                                                                               _message );

        // setup KomodoObject
        final KomodoObject kobject = _repo.add( uow, null, "kobject", null );
        kobject.setProperty( uow, propName, 8 );
        uow.commit();

        // test
        final Result result = rule.evaluate( null, kobject );
        assertThat( result.isOK(), is( true ) );
        assertThat( result.getPath(), is( kobject.getAbsolutePath() ) );
        assertThat( result.getRuleId(), is( rule.getName() ) );
    }

    @Test
    public void shouldVerifyPropertyRelationshipRuleFailsWhenChildDoesNotExist() throws Exception {
        final String propName = "sledge";
        final String[] propsExists = new String[] { "tko:text", "tko:type" };
        final String[] propsAbsent = new String[] { "tko:jcrName", "tko:ruleType" };
        final String[] childExists = new String[] { "tko:schemas", "tko:dataSources" };
        final String[] childAbsent = new String[] { "tko:vdbModels", "tko:vdbEntries" };
        final UnitOfWork uow = _repo.createTransaction( this.name.getMethodName(), false, null );

        // create rule in repo
        final Rule rule = _validationMgr.addPropertyRelationshipValidationRule( uow,
                                                                                "propRule",
                                                                                "nt:unstructured",
                                                                                propName,
                                                                                Arrays.asList( propsExists ),
                                                                                Arrays.asList( propsAbsent ),
                                                                                Arrays.asList( childExists ),
                                                                                Arrays.asList( childAbsent ),
                                                                                _description,
                                                                                _message );

        // setup KomodoObject
        final KomodoObject kobject = _repo.add( uow, null, "kobject", null );
        kobject.setProperty( uow, propName, "elvis" );
        kobject.setProperty( uow, propsExists[0], "foo" );
        kobject.setProperty( uow, propsExists[1], "foo" );
        kobject.addChild( uow, "larry", childExists[0] );
        uow.commit();

        // test
        final Result result = rule.evaluate( null, kobject );
        assertThat( result.getLevel(), is( Level.ERROR ) );
        assertThat( result.getPath(), is( kobject.getAbsolutePath() ) );
        assertThat( result.getRuleId(), is( rule.getName() ) );
    }

    @Test
    public void shouldVerifyPropertyRelationshipRuleFailsWhenChildShouldNotExist() throws Exception {
        final String propName = "sledge";
        final String[] propsExists = new String[] { "tko:text", "tko:type" };
        final String[] propsAbsent = new String[] { "tko:jcrName", "tko:ruleType" };
        final String[] childExists = new String[] { "tko:schemas" };
        final String[] childAbsent = new String[] { "tko:vdbModels", "tko:vdbEntries", "tko:dataSources" };
        final UnitOfWork uow = _repo.createTransaction( this.name.getMethodName(), false, null );

        // create rule in repo
        final Rule rule = _validationMgr.addPropertyRelationshipValidationRule( uow,
                                                                                "propRule",
                                                                                "nt:unstructured",
                                                                                propName,
                                                                                Arrays.asList( propsExists ),
                                                                                Arrays.asList( propsAbsent ),
                                                                                Arrays.asList( childExists ),
                                                                                Arrays.asList( childAbsent ),
                                                                                _description,
                                                                                _message );

        // setup KomodoObject
        final KomodoObject kobject = _repo.add( uow, null, "kobject", null );
        kobject.setProperty( uow, propName, "elvis" );
        kobject.setProperty( uow, propsExists[0], "foo" );
        kobject.setProperty( uow, propsExists[1], "bar" );
        kobject.addChild( uow, "larry", childExists[0] );
        kobject.addChild( uow, "bird", childAbsent[2] );
        uow.commit();

        // test
        final Result result = rule.evaluate( null, kobject );
        assertThat( result.getLevel(), is( Level.ERROR ) );
        assertThat( result.getPath(), is( kobject.getAbsolutePath() ) );
        assertThat( result.getRuleId(), is( rule.getName() ) );
    }

    @Test
    public void shouldVerifyPropertyRelationshipRuleFailsWhenPropertyDoesNotExist() throws Exception {
        final String propName = "sledge";
        final String[] propsExists = new String[] { "tko:text", "tko:type" };
        final String[] propsAbsent = new String[] { "tko:jcrName", "tko:ruleType" };
        final String[] childExists = new String[] { "tko:schemas", "tko:dataSources" };
        final String[] childAbsent = new String[] { "tko:vdbModels", "tko:vdbEntries" };
        final UnitOfWork uow = _repo.createTransaction( this.name.getMethodName(), false, null );

        // create rule in repo
        final Rule rule = _validationMgr.addPropertyRelationshipValidationRule( uow,
                                                                                "propRule",
                                                                                "nt:unstructured",
                                                                                propName,
                                                                                Arrays.asList( propsExists ),
                                                                                Arrays.asList( propsAbsent ),
                                                                                Arrays.asList( childExists ),
                                                                                Arrays.asList( childAbsent ),
                                                                                _description,
                                                                                _message );

        // setup KomodoObject
        final KomodoObject kobject = _repo.add( uow, null, "kobject", null );
        kobject.setProperty( uow, propName, "elvis" );
        kobject.setProperty( uow, propsExists[0], "foo" );
        kobject.addChild( uow, "larry", childExists[0] );
        kobject.addChild( uow, "bird", childExists[1] );
        uow.commit();

        // test
        final Result result = rule.evaluate( null, kobject );
        assertThat( result.getLevel(), is( Level.ERROR ) );
        assertThat( result.getPath(), is( kobject.getAbsolutePath() ) );
        assertThat( result.getRuleId(), is( rule.getName() ) );
    }

    @Test
    public void shouldVerifyPropertyRelationshipRuleFailsWhenPropertyShouldNotExist() throws Exception {
        final String propName = "sledge";
        final String[] propsExists = new String[] { "tko:text" };
        final String[] propsAbsent = new String[] { "tko:jcrName", "tko:ruleType", "tko:type" };
        final String[] childExists = new String[] { "tko:schemas", "tko:dataSources" };
        final String[] childAbsent = new String[] { "tko:vdbModels", "tko:vdbEntries" };
        final UnitOfWork uow = _repo.createTransaction( this.name.getMethodName(), false, null );

        // create rule in repo
        final Rule rule = _validationMgr.addPropertyRelationshipValidationRule( uow,
                                                                                "propRule",
                                                                                "nt:unstructured",
                                                                                propName,
                                                                                Arrays.asList( propsExists ),
                                                                                Arrays.asList( propsAbsent ),
                                                                                Arrays.asList( childExists ),
                                                                                Arrays.asList( childAbsent ),
                                                                                _description,
                                                                                _message );

        // setup KomodoObject
        final KomodoObject kobject = _repo.add( uow, null, "kobject", null );
        kobject.setProperty( uow, propName, "elvis" );
        kobject.setProperty( uow, propsExists[0], "foo" );
        kobject.setProperty( uow, propsAbsent[2], "bar" );
        kobject.addChild( uow, "larry", childExists[0] );
        kobject.addChild( uow, "bird", childExists[1] );
        uow.commit();

        // test
        final Result result = rule.evaluate( null, kobject );
        assertThat( result.getLevel(), is( Level.ERROR ) );
        assertThat( result.getPath(), is( kobject.getAbsolutePath() ) );
        assertThat( result.getRuleId(), is( rule.getName() ) );
    }

    @Test
    public void shouldVerifyPropertyRelationshipRulePasses() throws Exception {
        final String propName = "sledge";
        final String[] propsExists = new String[] { "tko:text", "tko:type" };
        final String[] propsAbsent = new String[] { "tko:jcrName", "tko:ruleType" };
        final String[] childExists = new String[] { "tko:schemas", "tko:dataSources" };
        final String[] childAbsent = new String[] { "tko:vdbModels", "tko:vdbEntries" };
        final UnitOfWork uow = _repo.createTransaction( this.name.getMethodName(), false, null );

        // create rule in repo
        final Rule rule = _validationMgr.addPropertyRelationshipValidationRule( uow,
                                                                                "propRule",
                                                                                "nt:unstructured",
                                                                                propName,
                                                                                Arrays.asList( propsExists ),
                                                                                Arrays.asList( propsAbsent ),
                                                                                Arrays.asList( childExists ),
                                                                                Arrays.asList( childAbsent ),
                                                                                _description,
                                                                                _message );

        // setup KomodoObject
        final KomodoObject kobject = _repo.add( uow, null, "kobject", null );
        kobject.setProperty( uow, propName, "elvis" );
        kobject.setProperty( uow, propsExists[0], "foo" );
        kobject.setProperty( uow, propsExists[1], "bar" );
        kobject.addChild( uow, "larry", childExists[0] );
        kobject.addChild( uow, "bird", childExists[1] );
        uow.commit();

        // test
        final Result result = rule.evaluate( null, kobject );
        assertThat( result.isOK(), is( true ) );
        assertThat( result.getPath(), is( kobject.getAbsolutePath() ) );
        assertThat( result.getRuleId(), is( rule.getName() ) );
    }

    @Test
    public void shouldVerifyPropertyValueRuleFailsWhenValueDoesNotMatchPattern() throws Exception {
        final String propName = "sledge";
        final UnitOfWork uow = _repo.createTransaction( this.name.getMethodName(), false, null );

        // create rule in repo
        final String pattern = "[0-9]{3}-[0-9]{2}-[0-9]{4}";
        final Rule rule = _validationMgr.addPropertyPatternRule( uow,
                                                                 "propRule",
                                                                 "nt:unstructured",
                                                                 propName,
                                                                 pattern,
                                                                 _description,
                                                                 _message );

        // setup KomodoObject
        final KomodoObject kobject = _repo.add( uow, null, "kobject", null );
        kobject.setProperty( uow, propName, "elvis" );
        uow.commit();

        // test
        final Result result = rule.evaluate( null, kobject );
        assertThat( result.getLevel(), is( Level.ERROR ) );
        assertThat( result.getPath(), is( kobject.getAbsolutePath() ) );
        assertThat( result.getRuleId(), is( rule.getName() ) );
    }

    @Test
    public void shouldVerifyPropertyValueRulePassesWhenValueMatchesPattern() throws Exception {
        final String propName = "sledge";
        final UnitOfWork uow = _repo.createTransaction( this.name.getMethodName(), false, null );

        // create rule in repo
        final String pattern = "[0-9]{3}-[0-9]{2}-[0-9]{4}";
        final Rule rule = _validationMgr.addPropertyPatternRule( uow,
                                                                 "propRule",
                                                                 "nt:unstructured",
                                                                 propName,
                                                                 pattern,
                                                                 _description,
                                                                 _message );

        // setup KomodoObject
        final KomodoObject kobject = _repo.add( uow, null, "kobject", null );
        kobject.setProperty( uow, propName, "123-45-6789" );
        uow.commit();

        // test
        final Result result = rule.evaluate( null, kobject );
        assertThat( result.isOK(), is( true ) );
        assertThat( result.getPath(), is( kobject.getAbsolutePath() ) );
        assertThat( result.getRuleId(), is( rule.getName() ) );
    }

    @Test
    public void shouldVerifyRequiredRuleFailsWhenChildNodeDoesNotExist() throws Exception {
        final String childType = "nt:file";
        final UnitOfWork uow = _repo.createTransaction( this.name.getMethodName(), false, null );

        // create rule in repo
        final Rule rule = _validationMgr.addChildTypeRequiredRule( uow,
                                                                   "childRule",
                                                                   "nt:unstructured",
                                                                   childType,
                                                                   _description,
                                                                   _message );
        // setup KomodoObject
        final KomodoObject kobject = _repo.add( uow, null, "kobject", null );
        kobject.addChild( uow, "blah", "nt:unstructured" ); // not the required child type
        uow.commit();

        // test
        final Result result = rule.evaluate( null, kobject );
        assertThat( result.getLevel(), is( Level.ERROR ) );
        assertThat( result.getPath(), is( kobject.getAbsolutePath() ) );
        assertThat( result.getRuleId(), is( rule.getName() ) );
    }

    @Test
    public void shouldVerifyRequiredRuleFailsWhenPropertyDoesNotExist() throws Exception {
        final String propName = "sledge";
        final UnitOfWork uow = _repo.createTransaction( this.name.getMethodName(), false, null );

        // create rule in repo
        final Rule rule = _validationMgr.addPropertyRequiredRule( uow,
                                                                  "propRule",
                                                                  "nt:unstructured",
                                                                  propName,
                                                                  _description,
                                                                  _message );

        // setup KomodoObject
        final KomodoObject kobject = _repo.add( uow, null, "kobject", null );
        uow.commit();

        // test
        final Result result = rule.evaluate( null, kobject );
        assertThat( result.getLevel(), is( Level.ERROR ) );
        assertThat( result.getPath(), is( kobject.getAbsolutePath() ) );
        assertThat( result.getRuleId(), is( rule.getName() ) );
    }

    @Test
    public void shouldVerifyRequiredRulePassesWhenChildNodeExists() throws Exception {
        final String childType = "nt:unstructured";
        final UnitOfWork uow = _repo.createTransaction( this.name.getMethodName(), false, null );

        // create rule in repo
        final Rule rule = _validationMgr.addChildTypeRequiredRule( uow,
                                                                   "childRule",
                                                                   "nt:unstructured",
                                                                   childType,
                                                                   _description,
                                                                   _message );
        // setup KomodoObject
        final KomodoObject kobject = _repo.add( uow, null, "kobject", null );
        kobject.addChild( uow, "blah", childType );
        uow.commit();

        // test
        final Result result = rule.evaluate( null, kobject );
        assertThat( result.isOK(), is( true ) );
        assertThat( result.getPath(), is( kobject.getAbsolutePath() ) );
        assertThat( result.getRuleId(), is( rule.getName() ) );
    }

    @Test
    public void shouldVerifyRequiredRulePassesWhenPropertyExists() throws Exception {
        final String propName = "sledge";
        final UnitOfWork uow = _repo.createTransaction( this.name.getMethodName(), false, null );

        // create rule in repo
        final Rule rule = _validationMgr.addPropertyRequiredRule( uow,
                                                                  "propRule",
                                                                  "nt:unstructured",
                                                                  propName,
                                                                  _description,
                                                                  _message );

        // setup KomodoObject
        final KomodoObject kobject = _repo.add( uow, null, "kobject", null );
        kobject.setProperty( uow, propName, "hammer" );
        uow.commit();

        // test
        final Result result = rule.evaluate( null, kobject );
        assertThat( result.isOK(), is( true ) );
        assertThat( result.getPath(), is( kobject.getAbsolutePath() ) );
        assertThat( result.getRuleId(), is( rule.getName() ) );
    }

}
