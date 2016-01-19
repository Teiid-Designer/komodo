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
import org.junit.Ignore;
import org.junit.Test;
import org.komodo.spi.KException;
import org.komodo.spi.outcome.Outcome.Level;
import org.komodo.spi.repository.KomodoObject;
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

    @Test( expected = IllegalArgumentException.class )
    public void shouldFailWhenNullRulesFile() throws Exception {
        _validationMgr.validateRules( null );
    }

    @Test
    public void shouldFindErrorsInXmlWithErrors() throws Exception {
        final int numErrors = 12;
        String testFilePath = getClass().getClassLoader().getResource("verifyValidationErrors.xml").getFile();
        final File testFile = new File(testFilePath);
        final List< String > errors = _validationMgr.validateRules( testFile );
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
    @Ignore("Needs to be fixed")
    public void shouldFindErrorsInXmlWithUniquenessProblems() throws Exception {
        final int numErrors = 16;
        String testFilePath = getClass().getClassLoader().getResource("verifyUniquenessChecks.xml").getFile();
        final File testFile = new File(testFilePath);
        final List< String > errors = _validationMgr.validateRules( testFile );
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
    	String testFilePath = getClass().getClassLoader().getResource("validationAllConstructs.xml").getFile();
        final File testFile = new File(testFilePath);
        final List< String > errors = _validationMgr.validateRules( testFile );
        assertThat( errors.isEmpty(), is( true ) );
    }

    @Test
    public void shouldVerifyChildCountRuleFailsWhenCountEqualsMaxButMaxNotInclusive() throws Exception {
        final String childType = "nt:unstructured";

        // create rule in repo
        final Number minValue = 1;
        final Number maxValue = 3;
        final Rule rule = _validationMgr.addChildCountValidationRule( getTransaction(),
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
        final KomodoObject kobject = _repo.add( getTransaction(), null, "kobject", null );

        for ( int i = -1; i < maxValue.intValue(); ++i ) {
            kobject.addChild( getTransaction(), ( "child" + i ), null );
        }

        commit();

        // test
        final Result result = rule.evaluate( getTransaction(), kobject );
        assertThat( result.getLevel(), is( Level.ERROR ) );
        assertThat( result.getPath(), is( kobject.getAbsolutePath() ) );
        assertThat( result.getRuleId(), is( rule.getName( getTransaction() ) ) );
    }

    @Test
    public void shouldVerifyChildCountRuleFailsWhenCountEqualsMinButMinNotInclusive() throws Exception {
        final String childType = "nt:unstructured";

        // create rule in repo
        final Number minValue = 1;
        final Number maxValue = 3;
        final Rule rule = _validationMgr.addChildCountValidationRule( getTransaction(),
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
        final KomodoObject kobject = _repo.add( getTransaction(), null, "kobject", null );

        for ( int i = 0; i < minValue.intValue(); ++i ) {
            kobject.addChild( getTransaction(), ( "child" + i ), null );
        }

        commit();

        // test
        final Result result = rule.evaluate( getTransaction(), kobject );
        assertThat( result.getLevel(), is( Level.ERROR ) );
        assertThat( result.getPath(), is( kobject.getAbsolutePath() ) );
        assertThat( result.getRuleId(), is( rule.getName( getTransaction() ) ) );
    }

    @Test
    public void shouldVerifyChildCountRuleFailsWhenCountIsNotWithinRange() throws Exception {
        final String childType = "nt:unstructured";

        // create rule in repo
        final Number minValue = 1;
        final Number maxValue = 3;
        final Rule rule = _validationMgr.addChildCountValidationRule( getTransaction(),
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
        final KomodoObject kobject = _repo.add( getTransaction(), null, "kobject", null );

        for ( int i = -1; i < ( maxValue.intValue() + 1 ); ++i ) {
            kobject.addChild( getTransaction(), ( "child" + i ), null );
        }

        commit();

        // test
        final Result result = rule.evaluate( getTransaction(), kobject );
        assertThat( result.getLevel(), is( Level.ERROR ) );
        assertThat( result.getPath(), is( kobject.getAbsolutePath() ) );
        assertThat( result.getRuleId(), is( rule.getName( getTransaction() ) ) );
    }

    @Test
    public void shouldVerifyChildCountRulePassesWhenCountWithinRange() throws Exception {
        final String childType = "nt:unstructured";

        // create rule in repo
        final Number minValue = 1;
        final Number maxValue = 3;
        final Rule rule = _validationMgr.addChildCountValidationRule( getTransaction(),
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
        final KomodoObject kobject = _repo.add( getTransaction(), null, "kobject", null );

        for ( int i = 0; i < maxValue.intValue(); ++i ) {
            kobject.addChild( getTransaction(), ( "child" + i ), null );
        }

        commit();

        // test
        final Result result = rule.evaluate( getTransaction(), kobject );
        assertThat( result.isOK(), is( true ) );
        assertThat( result.getPath(), is( kobject.getAbsolutePath() ) );
        assertThat( result.getRuleId(), is( rule.getName( getTransaction() ) ) );
    }

    @Test( expected = KException.class )
    public void shouldVerifyDisabledRulesCannotBeEvaluated() throws Exception {
        final String propName = "sledge";

        // create rule in repo
        final Rule rule = _validationMgr.addPropertyRequiredRule( getTransaction(),
                                                                  "propRule",
                                                                  "nt:unstructured",
                                                                  propName,
                                                                  _description,
                                                                  _message );

        // setup KomodoObject
        final KomodoObject kobject = _repo.add( getTransaction(), null, "kobject", null );
        commit();

        // disable rule
        rule.setEnabled( getTransaction(), false );

        // test
        rule.evaluate( getTransaction(), kobject );
    }

    @Test
    public void shouldVerifyNodeNameRuleFailsWhenNameDoesNotMatchesPattern() throws Exception {
        // create rule in repo
        final String pattern = "[A-Z]{3}";
        final Rule rule = _validationMgr.addNodeNameRule( getTransaction(),
                                                          "propRule",
                                                          "nt:unstructured",
                                                          pattern,
                                                          _description,
                                                          _message );

        // setup KomodoObject
        final KomodoObject kobject = _repo.add( getTransaction(), null, "ABc", null );
        commit();

        // test
        final Result result = rule.evaluate( getTransaction(), kobject );
        assertThat( result.getLevel(), is( Level.ERROR ) );
        assertThat( result.getPath(), is( kobject.getAbsolutePath() ) );
        assertThat( result.getRuleId(), is( rule.getName( getTransaction() ) ) );
    }

    @Test
    public void shouldVerifyNodeNameRulePassesWhenNameMatchesPattern() throws Exception {
        // create rule in repo
        final String pattern = "[A-Z]{3}";
        final Rule rule = _validationMgr.addNodeNameRule( getTransaction(),
                                                          "propRule",
                                                          "nt:unstructured",
                                                          pattern,
                                                          _description,
                                                          _message );

        // setup KomodoObject
        final KomodoObject kobject = _repo.add( getTransaction(), null, "ABC", null );
        commit();

        // test
        final Result result = rule.evaluate( getTransaction(), kobject );
        assertThat( result.isOK(), is( true ) );
        assertThat( result.getPath(), is( kobject.getAbsolutePath() ) );
        assertThat( result.getRuleId(), is( rule.getName( getTransaction() ) ) );
    }

    @Test
    public void shouldVerifyPropertyRangeRuleFailsWhenValueEqualsMaxButMaxNotInclusive() throws Exception {
        final String propName = "sledge";

        // create rule in repo
        final Number minValue = 5;
        final Number maxValue = 10;
        final Rule rule = _validationMgr.addPropertyValueNumberValidationRule( getTransaction(),
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
        final KomodoObject kobject = _repo.add( getTransaction(), null, "kobject", null );
        kobject.setProperty( getTransaction(), propName, maxValue );
        commit();

        // test
        final Result result = rule.evaluate( getTransaction(), kobject );
        assertThat( result.getLevel(), is( Level.ERROR ) );
        assertThat( result.getPath(), is( kobject.getAbsolutePath() ) );
        assertThat( result.getRuleId(), is( rule.getName( getTransaction() ) ) );
    }

    @Test
    public void shouldVerifyPropertyRangeRuleFailsWhenValueEqualsMinButMinNotInclusive() throws Exception {
        final String propName = "sledge";

        // create rule in repo
        final Number minValue = 5;
        final Number maxValue = 10;
        final Rule rule = _validationMgr.addPropertyValueNumberValidationRule( getTransaction(),
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
        final KomodoObject kobject = _repo.add( getTransaction(), null, "kobject", null );
        kobject.setProperty( getTransaction(), propName, minValue );
        commit();

        // test
        final Result result = rule.evaluate( getTransaction(), kobject );
        assertThat( result.getLevel(), is( Level.ERROR ) );
        assertThat( result.getPath(), is( kobject.getAbsolutePath() ) );
        assertThat( result.getRuleId(), is( rule.getName( getTransaction() ) ) );
    }

    @Test
    public void shouldVerifyPropertyRangeRulePassesWhenValueWithinRange() throws Exception {
        final String propName = "sledge";

        // create rule in repo
        final Number minValue = 5;
        final Number maxValue = 10;
        final Rule rule = _validationMgr.addPropertyValueNumberValidationRule( getTransaction(),
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
        final KomodoObject kobject = _repo.add( getTransaction(), null, "kobject", null );
        kobject.setProperty( getTransaction(), propName, 8 );
        commit();

        // test
        final Result result = rule.evaluate( getTransaction(), kobject );
        assertThat( result.isOK(), is( true ) );
        assertThat( result.getPath(), is( kobject.getAbsolutePath() ) );
        assertThat( result.getRuleId(), is( rule.getName( getTransaction() ) ) );
    }

    @Test
    public void shouldVerifyPropertyRelationshipRuleFailsWhenChildDoesNotExist() throws Exception {
        final String propName = "sledge";
        final String[] propsExists = new String[] { "tko:text", "tko:type" };
        final String[] propsAbsent = new String[] { "tko:jcrName", "tko:ruleType" };
        final String[] childExists = new String[] { "tko:schemas", "tko:dataSources" };
        final String[] childAbsent = new String[] { "tko:vdbModels", "tko:vdbEntries" };

        // create rule in repo
        final Rule rule = _validationMgr.addPropertyRelationshipValidationRule( getTransaction(),
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
        final KomodoObject kobject = _repo.add( getTransaction(), null, "kobject", null );
        kobject.setProperty( getTransaction(), propName, "elvis" );
        kobject.setProperty( getTransaction(), propsExists[0], "foo" );
        kobject.setProperty( getTransaction(), propsExists[1], "foo" );
        kobject.addChild( getTransaction(), "larry", childExists[0] );
        commit();

        // test
        final Result result = rule.evaluate( getTransaction(), kobject );
        assertThat( result.getLevel(), is( Level.ERROR ) );
        assertThat( result.getPath(), is( kobject.getAbsolutePath() ) );
        assertThat( result.getRuleId(), is( rule.getName( getTransaction() ) ) );
    }

    @Test
    public void shouldVerifyPropertyRelationshipRuleFailsWhenChildShouldNotExist() throws Exception {
        final String propName = "sledge";
        final String[] propsExists = new String[] { "tko:text", "tko:type" };
        final String[] propsAbsent = new String[] { "tko:jcrName", "tko:ruleType" };
        final String[] childExists = new String[] { "tko:schemas" };
        final String[] childAbsent = new String[] { "tko:vdbModels", "tko:vdbEntries", "tko:dataSources" };

        // create rule in repo
        final Rule rule = _validationMgr.addPropertyRelationshipValidationRule( getTransaction(),
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
        final KomodoObject kobject = _repo.add( getTransaction(), null, "kobject", null );
        kobject.setProperty( getTransaction(), propName, "elvis" );
        kobject.setProperty( getTransaction(), propsExists[0], "foo" );
        kobject.setProperty( getTransaction(), propsExists[1], "bar" );
        kobject.addChild( getTransaction(), "larry", childExists[0] );
        kobject.addChild( getTransaction(), "bird", childAbsent[2] );
        commit();

        // test
        final Result result = rule.evaluate( getTransaction(), kobject );
        assertThat( result.getLevel(), is( Level.ERROR ) );
        assertThat( result.getPath(), is( kobject.getAbsolutePath() ) );
        assertThat( result.getRuleId(), is( rule.getName( getTransaction() ) ) );
    }

    @Test
    public void shouldVerifyPropertyRelationshipRuleFailsWhenPropertyDoesNotExist() throws Exception {
        final String propName = "sledge";
        final String[] propsExists = new String[] { "tko:text", "tko:type" };
        final String[] propsAbsent = new String[] { "tko:jcrName", "tko:ruleType" };
        final String[] childExists = new String[] { "tko:schemas", "tko:dataSources" };
        final String[] childAbsent = new String[] { "tko:vdbModels", "tko:vdbEntries" };

        // create rule in repo
        final Rule rule = _validationMgr.addPropertyRelationshipValidationRule( getTransaction(),
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
        final KomodoObject kobject = _repo.add( getTransaction(), null, "kobject", null );
        kobject.setProperty( getTransaction(), propName, "elvis" );
        kobject.setProperty( getTransaction(), propsExists[0], "foo" );
        kobject.addChild( getTransaction(), "larry", childExists[0] );
        kobject.addChild( getTransaction(), "bird", childExists[1] );
        commit();

        // test
        final Result result = rule.evaluate( getTransaction(), kobject );
        assertThat( result.getLevel(), is( Level.ERROR ) );
        assertThat( result.getPath(), is( kobject.getAbsolutePath() ) );
        assertThat( result.getRuleId(), is( rule.getName( getTransaction() ) ) );
    }

    @Test
    public void shouldVerifyPropertyRelationshipRuleFailsWhenPropertyShouldNotExist() throws Exception {
        final String propName = "sledge";
        final String[] propsExists = new String[] { "tko:text" };
        final String[] propsAbsent = new String[] { "tko:jcrName", "tko:ruleType", "tko:type" };
        final String[] childExists = new String[] { "tko:schemas", "tko:dataSources" };
        final String[] childAbsent = new String[] { "tko:vdbModels", "tko:vdbEntries" };

        // create rule in repo
        final Rule rule = _validationMgr.addPropertyRelationshipValidationRule( getTransaction(),
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
        final KomodoObject kobject = _repo.add( getTransaction(), null, "kobject", null );
        kobject.setProperty( getTransaction(), propName, "elvis" );
        kobject.setProperty( getTransaction(), propsExists[0], "foo" );
        kobject.setProperty( getTransaction(), propsAbsent[2], "bar" );
        kobject.addChild( getTransaction(), "larry", childExists[0] );
        kobject.addChild( getTransaction(), "bird", childExists[1] );
        commit();

        // test
        final Result result = rule.evaluate( getTransaction(), kobject );
        assertThat( result.getLevel(), is( Level.ERROR ) );
        assertThat( result.getPath(), is( kobject.getAbsolutePath() ) );
        assertThat( result.getRuleId(), is( rule.getName( getTransaction() ) ) );
    }

    @Test
    public void shouldVerifyPropertyRelationshipRulePasses() throws Exception {
        final String propName = "sledge";
        final String[] propsExists = new String[] { "tko:text", "tko:type" };
        final String[] propsAbsent = new String[] { "tko:jcrName", "tko:ruleType" };
        final String[] childExists = new String[] { "tko:schemas", "tko:dataSources" };
        final String[] childAbsent = new String[] { "tko:vdbModels", "tko:vdbEntries" };

        // create rule in repo
        final Rule rule = _validationMgr.addPropertyRelationshipValidationRule( getTransaction(),
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
        final KomodoObject kobject = _repo.add( getTransaction(), null, "kobject", null );
        kobject.setProperty( getTransaction(), propName, "elvis" );
        kobject.setProperty( getTransaction(), propsExists[0], "foo" );
        kobject.setProperty( getTransaction(), propsExists[1], "bar" );
        kobject.addChild( getTransaction(), "larry", childExists[0] );
        kobject.addChild( getTransaction(), "bird", childExists[1] );
        commit();

        // test
        final Result result = rule.evaluate( getTransaction(), kobject );
        assertThat( result.isOK(), is( true ) );
        assertThat( result.getPath(), is( kobject.getAbsolutePath() ) );
        assertThat( result.getRuleId(), is( rule.getName( getTransaction() ) ) );
    }

    @Test
    public void shouldVerifyPropertyValueRuleFailsWhenValueDoesNotMatchPattern() throws Exception {
        final String propName = "sledge";

        // create rule in repo
        final String pattern = "[0-9]{3}-[0-9]{2}-[0-9]{4}";
        final Rule rule = _validationMgr.addPropertyPatternRule( getTransaction(),
                                                                 "propRule",
                                                                 "nt:unstructured",
                                                                 propName,
                                                                 pattern,
                                                                 _description,
                                                                 _message );

        // setup KomodoObject
        final KomodoObject kobject = _repo.add( getTransaction(), null, "kobject", null );
        kobject.setProperty( getTransaction(), propName, "elvis" );
        commit();

        // test
        final Result result = rule.evaluate( getTransaction(), kobject );
        assertThat( result.getLevel(), is( Level.ERROR ) );
        assertThat( result.getPath(), is( kobject.getAbsolutePath() ) );
        assertThat( result.getRuleId(), is( rule.getName( getTransaction() ) ) );
    }

    @Test
    public void shouldVerifyPropertyValueRulePassesWhenValueMatchesPattern() throws Exception {
        final String propName = "sledge";

        // create rule in repo
        final String pattern = "[0-9]{3}-[0-9]{2}-[0-9]{4}";
        final Rule rule = _validationMgr.addPropertyPatternRule( getTransaction(),
                                                                 "propRule",
                                                                 "nt:unstructured",
                                                                 propName,
                                                                 pattern,
                                                                 _description,
                                                                 _message );

        // setup KomodoObject
        final KomodoObject kobject = _repo.add( getTransaction(), null, "kobject", null );
        kobject.setProperty( getTransaction(), propName, "123-45-6789" );
        commit();

        // test
        final Result result = rule.evaluate( getTransaction(), kobject );
        assertThat( result.isOK(), is( true ) );
        assertThat( result.getPath(), is( kobject.getAbsolutePath() ) );
        assertThat( result.getRuleId(), is( rule.getName( getTransaction() ) ) );
    }

    @Test
    public void shouldVerifyRequiredRuleFailsWhenChildNodeDoesNotExist() throws Exception {
        final String childType = "nt:file";

        // create rule in repo
        final Rule rule = _validationMgr.addChildTypeRequiredRule( getTransaction(),
                                                                   "childRule",
                                                                   "nt:unstructured",
                                                                   childType,
                                                                   _description,
                                                                   _message );
        // setup KomodoObject
        final KomodoObject kobject = _repo.add( getTransaction(), null, "kobject", null );
        kobject.addChild( getTransaction(), "blah", "nt:unstructured" ); // not the required child type
        commit();

        // test
        final Result result = rule.evaluate( getTransaction(), kobject );
        assertThat( result.getLevel(), is( Level.ERROR ) );
        assertThat( result.getPath(), is( kobject.getAbsolutePath() ) );
        assertThat( result.getRuleId(), is( rule.getName( getTransaction() ) ) );
    }

    @Test
    public void shouldVerifyRequiredRuleFailsWhenPropertyDoesNotExist() throws Exception {
        final String propName = "sledge";

        // create rule in repo
        final Rule rule = _validationMgr.addPropertyRequiredRule( getTransaction(),
                                                                  "propRule",
                                                                  "nt:unstructured",
                                                                  propName,
                                                                  _description,
                                                                  _message );

        // setup KomodoObject
        final KomodoObject kobject = _repo.add( getTransaction(), null, "kobject", null );
        commit();

        // test
        final Result result = rule.evaluate( getTransaction(), kobject );
        assertThat( result.getLevel(), is( Level.ERROR ) );
        assertThat( result.getPath(), is( kobject.getAbsolutePath() ) );
        assertThat( result.getRuleId(), is( rule.getName( getTransaction() ) ) );
    }

    @Test
    public void shouldVerifyRequiredRulePassesWhenChildNodeExists() throws Exception {
        final String childType = "nt:unstructured";

        // create rule in repo
        final Rule rule = _validationMgr.addChildTypeRequiredRule( getTransaction(),
                                                                   "childRule",
                                                                   "nt:unstructured",
                                                                   childType,
                                                                   _description,
                                                                   _message );
        // setup KomodoObject
        final KomodoObject kobject = _repo.add( getTransaction(), null, "kobject", null );
        kobject.addChild( getTransaction(), "blah", childType );
        commit();

        // test
        final Result result = rule.evaluate( getTransaction(), kobject );
        assertThat( result.isOK(), is( true ) );
        assertThat( result.getPath(), is( kobject.getAbsolutePath() ) );
        assertThat( result.getRuleId(), is( rule.getName( getTransaction() ) ) );
    }

    @Test
    public void shouldVerifyRequiredRulePassesWhenPropertyExists() throws Exception {
        final String propName = "sledge";

        // create rule in repo
        final Rule rule = _validationMgr.addPropertyRequiredRule( getTransaction(),
                                                                  "propRule",
                                                                  "nt:unstructured",
                                                                  propName,
                                                                  _description,
                                                                  _message );

        // setup KomodoObject
        final KomodoObject kobject = _repo.add( getTransaction(), null, "kobject", null );
        kobject.setProperty( getTransaction(), propName, "hammer" );
        commit();

        // test
        final Result result = rule.evaluate( getTransaction(), kobject );
        assertThat( result.isOK(), is( true ) );
        assertThat( result.getPath(), is( kobject.getAbsolutePath() ) );
        assertThat( result.getRuleId(), is( rule.getName( getTransaction() ) ) );
    }

}
