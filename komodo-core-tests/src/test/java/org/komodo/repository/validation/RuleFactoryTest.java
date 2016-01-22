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
import java.util.Arrays;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import org.junit.BeforeClass;
import org.junit.Test;
import org.komodo.spi.KException;
import org.komodo.spi.outcome.Outcome.Level;
import org.komodo.spi.repository.KomodoObject;
import org.komodo.spi.repository.validation.Result;
import org.komodo.spi.repository.validation.Rule;
import org.komodo.spi.repository.validation.Rule.MessageKey;
import org.komodo.spi.utils.LocalizedMessage;
import org.komodo.test.utils.AbstractLocalRepositoryTest;

@SuppressWarnings( { "javadoc", "nls" } )
public final class RuleFactoryTest extends AbstractLocalRepositoryTest {

    private static List< LocalizedMessage > _description;
    private static List< LocalizedMessage > _message;

    @BeforeClass
    public static void oneTimeSetup() throws Exception {
        _description = Collections.singletonList( new LocalizedMessage( MessageKey.DESCRIPTION.name(),
                                                                        "en",
                                                                        "This is a rule description" ) );
        _message = Collections.singletonList( new LocalizedMessage( "myrule.msg", "en", "This is a rule error message" ) );
    }

    @Test
    public void shouldVerifyChildCountRuleFailsWhenCountEqualsMaxButMaxNotInclusive() throws Exception {
        final String childType = "nt:unstructured";

        // create rule in repo
        final Number minValue = 1;
        final Number maxValue = 3;
        final Rule rule = RuleFactory.createChildCountValidationRule( getTransaction(),
                                                                      _repo,
                                                                      "propRule",
                                                                      "nt:unstructured",
                                                                      new HashMap<String,String>(),
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
        final Rule rule = RuleFactory.createChildCountValidationRule( getTransaction(),
                                                                      _repo,
                                                                      "propRule",
                                                                      "nt:unstructured",
                                                                      new HashMap<String,String>(),
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
        final Rule rule = RuleFactory.createChildCountValidationRule( getTransaction(),
                                                                      _repo,
                                                                      "propRule",
                                                                      "nt:unstructured",
                                                                      new HashMap<String,String>(),
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
        final Rule rule = RuleFactory.createChildCountValidationRule( getTransaction(),
                                                                      _repo,
                                                                      "propRule",
                                                                      "nt:unstructured",
                                                                      new HashMap<String,String>(),
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
        final Rule rule = RuleFactory.createPropertyRequiredRule( getTransaction(),
                                                                  _repo,
                                                                  "propRule",
                                                                  "nt:unstructured",
                                                                  new HashMap<String,String>(),
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
        final Rule rule = RuleFactory.createNodeNameRule( getTransaction(),
                                                          _repo,
                                                          "propRule",
                                                          "nt:unstructured",
                                                          new HashMap<String,String>(),
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
        final Rule rule = RuleFactory.createNodeNameRule( getTransaction(),
                                                          _repo,
                                                          "propRule",
                                                          "nt:unstructured",
                                                          new HashMap<String,String>(),
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
        final Rule rule = RuleFactory.createPropertyValueNumberValidationRule( getTransaction(),
                                                                               _repo,
                                                                               "propRule",
                                                                               "nt:unstructured",
                                                                               new HashMap<String,String>(),
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
        final Rule rule = RuleFactory.createPropertyValueNumberValidationRule( getTransaction(),
                                                                               _repo,
                                                                               "propRule",
                                                                               "nt:unstructured",
                                                                               new HashMap<String,String>(),
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
        final Rule rule = RuleFactory.createPropertyValueNumberValidationRule( getTransaction(),
                                                                               _repo,
                                                                               "propRule",
                                                                               "nt:unstructured",
                                                                               new HashMap<String,String>(),
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
        final Rule rule = RuleFactory.createPropertyRelationshipValidationRule( getTransaction(),
                                                                                _repo,
                                                                                "propRule",
                                                                                "nt:unstructured",
                                                                                new HashMap<String,String>(),
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
        final Rule rule = RuleFactory.createPropertyRelationshipValidationRule( getTransaction(),
                                                                                _repo,
                                                                                "propRule",
                                                                                "nt:unstructured",
                                                                                new HashMap<String,String>(),
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
        final Rule rule = RuleFactory.createPropertyRelationshipValidationRule( getTransaction(),
                                                                                _repo,
                                                                                "propRule",
                                                                                "nt:unstructured",
                                                                                new HashMap<String,String>(),
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
        final Rule rule = RuleFactory.createPropertyRelationshipValidationRule( getTransaction(),
                                                                                _repo,
                                                                                "propRule",
                                                                                "nt:unstructured",
                                                                                new HashMap<String,String>(),
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
        final Rule rule = RuleFactory.createPropertyRelationshipValidationRule( getTransaction(),
                                                                                _repo,
                                                                                "propRule",
                                                                                "nt:unstructured",
                                                                                new HashMap<String,String>(),
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
        final Rule rule = RuleFactory.createPropertyPatternRule( getTransaction(),
                                                                 _repo,
                                                                 "propRule",
                                                                 "nt:unstructured",
                                                                 new HashMap<String,String>(),
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
        final Rule rule = RuleFactory.createPropertyPatternRule( getTransaction(),
                                                                 _repo,
                                                                 "propRule",
                                                                 "nt:unstructured",
                                                                 new HashMap<String,String>(),
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
        final Rule rule = RuleFactory.createChildTypeRequiredRule( getTransaction(),
                                                                   _repo,
                                                                   "childRule",
                                                                   "nt:unstructured",
                                                                   new HashMap<String,String>(),
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
        final Rule rule = RuleFactory.createPropertyRequiredRule( getTransaction(),
                                                                  _repo,
                                                                  "propRule",
                                                                  "nt:unstructured",
                                                                  new HashMap<String,String>(),
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
        final Rule rule = RuleFactory.createChildTypeRequiredRule( getTransaction(),
                                                                   _repo,
                                                                   "childRule",
                                                                   "nt:unstructured",
                                                                   new HashMap<String,String>(),
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
        final Rule rule = RuleFactory.createPropertyRequiredRule( getTransaction(),
                                                                  _repo,
                                                                  "propRule",
                                                                  "nt:unstructured",
                                                                  new HashMap<String,String>(),
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
    
    @Test
    public void shouldVerifyGetDescriptionOnFailure() throws Exception {
        // If no messages are supplied in the rule definition,
        // the rule failure message should be the rule 'description'
        
        final String propName = "sledge";

        // create rule in repo
        final Rule rule = RuleFactory.createPropertyRequiredRule( getTransaction(),
                                                                  _repo,
                                                                  "propRule",
                                                                  "nt:unstructured",
                                                                  new HashMap<String,String>(),
                                                                  propName,
                                                                  _description,
                                                                  null );

        // setup KomodoObject
        final KomodoObject kobject = _repo.add( getTransaction(), null, "kobject", null );
        commit();

        // test
        final Result result = rule.evaluate( getTransaction(), kobject );
        assertThat( result.getLevel(), is( Level.ERROR ) );
        assertThat( result.getPath(), is( kobject.getAbsolutePath() ) );
        assertThat( result.getRuleId(), is( rule.getName( getTransaction() ) ) );
        assertThat( result.getMessage().contains("This is a rule description"), is( true ) );
    }
    
    @Test
    public void shouldVerifyGetUserOverrideMessageOnFailure() throws Exception {
        // If message is supplied in the rule definition, the rule failure message
        // should be the 'message' - it overrides the 'description'
        
        final String propName = "sledge";
        String messageOverride = "This is a rule error message";
        _message = Collections.singletonList( new LocalizedMessage( MessageKey.REQUIRED_PROPERTY_NOT_FOUND.name(), "en", messageOverride ) );
        
        // create rule in repo
        final Rule rule = RuleFactory.createPropertyRequiredRule( getTransaction(),
                                                                  _repo,
                                                                  "propRule",
                                                                  "nt:unstructured",
                                                                  new HashMap<String,String>(),
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
        assertThat( result.getMessage().contains(messageOverride), is( true ) );
    }
        
}
