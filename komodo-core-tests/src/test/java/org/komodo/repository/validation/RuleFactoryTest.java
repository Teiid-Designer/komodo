/*
 * JBoss, Home of Professional Open Source.
 * See the COPYRIGHT.txt file distributed with this work for information
 * regarding copyright ownership.  Some portions may be licensed
 * to Red Hat, Inc. under one or more contributor license agreements.
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2.1 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
 * 02110-1301 USA.
 */
package org.komodo.repository.validation;

import static org.hamcrest.core.Is.is;
import static org.junit.Assert.assertThat;
import java.util.Arrays;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import org.junit.BeforeClass;
import org.junit.Test;
import org.komodo.spi.outcome.Outcome;
import org.komodo.spi.outcome.Outcome.Level;
import org.komodo.spi.repository.KomodoObject;
import org.komodo.spi.repository.validation.Result;
import org.komodo.spi.repository.validation.Rule;
import org.komodo.spi.repository.validation.Rule.MessageKey;
import org.komodo.spi.utils.LocalizedMessage;
import org.komodo.test.utils.AbstractLocalRepositoryTest;

@SuppressWarnings( { "javadoc", "nls" } )
public final class RuleFactoryTest extends AbstractLocalRepositoryTest {

    private static String RULE_DESCRIPTION = "This is a rule description";
    private static String RULE_MESSAGE = "This is a rule error message";

    private static List< LocalizedMessage > _description;
    private static List< LocalizedMessage > _message;

    @BeforeClass
    public static void oneTimeSetup() throws Exception {
        _description = Collections.singletonList( new LocalizedMessage( MessageKey.DESCRIPTION.name(),
                                                                        "en",
                                                                        RULE_DESCRIPTION ) );
        _message = Collections.singletonList( new LocalizedMessage( "myrule.msg", "en", RULE_MESSAGE ) );
    }

    @Test
    public void shouldVerifyChildCountRulePassesWhenCountWithinRange() throws Exception {
        final String ruleNodeType = "nt:unstructured";
        final String childType = "nt:unstructured";
        final boolean childRequired = true;
        final Map<String,String> nodePropRestrictions = new HashMap<String,String>();
        final Map<String,String> childPropRestrictions = new HashMap<String,String>();
        final Outcome.Level severity = Outcome.Level.ERROR;

        // create rule in repo
        final Number minValue = 1;
        final Number maxValue = 3;
        final Rule rule = RuleFactory.createChildCountValidationRule( getTransaction(),
                                                                      _repo,
                                                                      this.name.getMethodName(),
                                                                      ruleNodeType,
                                                                      nodePropRestrictions,
                                                                      childType,
                                                                      childPropRestrictions,
                                                                      childRequired,
                                                                      minValue,
                                                                      true,
                                                                      maxValue,
                                                                      true,
                                                                      severity,
                                                                      _description,
                                                                      _message );

        // setup KomodoObject
        final KomodoObject kobject = _repo.add( getTransaction(), null, "kobject", null );

        // adds maxValue children
        for ( int i = 0; i < maxValue.intValue(); ++i ) {
            kobject.addChild( getTransaction(), ( "child" + i ), "nt:unstructured" );
        }

        commit();

        // test
        final Result result = rule.evaluate( getTransaction(), kobject );
        assertThat( result.isOK(), is( true ) );
        assertThat( result.getPath(), is( kobject.getAbsolutePath() ) );
        assertThat( result.getRuleId(), is( rule.getName( getTransaction() ) ) );
    }

    @Test
    public void shouldVerifyChildCountRuleFailsWhenCountEqualsMaxButMaxNotInclusive() throws Exception {
        final String ruleNodeType = "nt:unstructured";
        final Map<String,String> nodePropRestrictions = new HashMap<String,String>();
        final Map<String,String> childPropRestrictions = new HashMap<String,String>();
        final String childType = "nt:unstructured";
        final boolean childRequired = true;
        final Outcome.Level severity = Outcome.Level.ERROR;

        // create rule in repo
        final Number minValue = 1;
        final Number maxValue = 3;
        final Rule rule = RuleFactory.createChildCountValidationRule( getTransaction(),
                                                                      _repo,
                                                                      this.name.getMethodName(),
                                                                      ruleNodeType,
                                                                      nodePropRestrictions,
                                                                      childType,
                                                                      childPropRestrictions,
                                                                      childRequired,
                                                                      minValue,
                                                                      true,
                                                                      maxValue,
                                                                      false,
                                                                      severity,
                                                                      _description,
                                                                      Collections.emptyList() );

        // setup KomodoObject
        final KomodoObject kobject = _repo.add( getTransaction(), null, "kobject", null );

        // adds maxValue children
        for ( int i = -1; i < maxValue.intValue(); ++i ) {
            kobject.addChild( getTransaction(), ( "child" + i ), null );
        }

        commit();

        // test
        final Result result = rule.evaluate( getTransaction(), kobject );
        assertThat( result.getLevel(), is( Level.ERROR ) );
        assertThat( result.getPath(), is( kobject.getAbsolutePath() ) );
        assertThat( result.getRuleId(), is( rule.getName( getTransaction() ) ) );
        assertThat( result.getMessage().contains(RULE_DESCRIPTION), is( true ) );
    }

    @Test
    public void shouldVerifyChildCountRuleFailsWhenCountEqualsMinButMinNotInclusive() throws Exception {
        final String ruleNodeType = "nt:unstructured";
        final Map<String,String> nodePropRestrictions = new HashMap<String,String>();
        final Map<String,String> childPropRestrictions = new HashMap<String,String>();
        final String childType = "nt:unstructured";
        final boolean childRequired = true;
        final Outcome.Level severity = Outcome.Level.ERROR;

        // create rule in repo
        final Number minValue = 1;
        final Number maxValue = 3;
        final Rule rule = RuleFactory.createChildCountValidationRule( getTransaction(),
                                                                      _repo,
                                                                      this.name.getMethodName(),
                                                                      ruleNodeType,
                                                                      nodePropRestrictions,
                                                                      childType,
                                                                      childPropRestrictions,
                                                                      childRequired,
                                                                      minValue,
                                                                      false,
                                                                      maxValue,
                                                                      true,
                                                                      severity,
                                                                      _description,
                                                                      Collections.emptyList() );

        // setup KomodoObject
        final KomodoObject kobject = _repo.add( getTransaction(), null, "kobject", null );

        // adds minValue children
        for ( int i = 0; i < minValue.intValue(); ++i ) {
            kobject.addChild( getTransaction(), ( "child" + i ), null );
        }

        commit();

        // test
        final Result result = rule.evaluate( getTransaction(), kobject );
        assertThat( result.getLevel(), is( Level.ERROR ) );
        assertThat( result.getPath(), is( kobject.getAbsolutePath() ) );
        assertThat( result.getRuleId(), is( rule.getName( getTransaction() ) ) );
        assertThat( result.getMessage().contains(RULE_DESCRIPTION), is( true ) );
    }

    @Test
    public void shouldVerifyChildCountRuleFailsWhenCountIsNotWithinRange() throws Exception {
        final String ruleNodeType = "nt:unstructured";
        final Map<String,String> nodePropRestrictions = new HashMap<String,String>();
        final Map<String,String> childPropRestrictions = new HashMap<String,String>();
        final String childType = "nt:unstructured";
        final boolean childRequired = true;
        final Outcome.Level severity = Outcome.Level.ERROR;

        // create rule in repo
        final Number minValue = 1;
        final Number maxValue = 3;
        final Rule rule = RuleFactory.createChildCountValidationRule( getTransaction(),
                                                                      _repo,
                                                                      this.name.getMethodName(),
                                                                      ruleNodeType,
                                                                      nodePropRestrictions,
                                                                      childType,
                                                                      childPropRestrictions,
                                                                      childRequired,
                                                                      minValue,
                                                                      true,
                                                                      maxValue,
                                                                      true,
                                                                      severity,
                                                                      _description,
                                                                      Collections.emptyList() );

        // setup KomodoObject
        final KomodoObject kobject = _repo.add( getTransaction(), null, "kobject", null );

        // adds minValue children
        for ( int i = -1; i < ( maxValue.intValue() + 1 ); ++i ) {
            kobject.addChild( getTransaction(), ( "child" + i ), null );
        }

        commit();

        // test
        final Result result = rule.evaluate( getTransaction(), kobject );
        assertThat( result.getLevel(), is( Level.ERROR ) );
        assertThat( result.getPath(), is( kobject.getAbsolutePath() ) );
        assertThat( result.getRuleId(), is( rule.getName( getTransaction() ) ) );
        assertThat( result.getMessage().contains(RULE_DESCRIPTION), is( true ) );
    }

    @Test
    public void shouldVerifyChildCountRuleFailsWhenChildIsRequiredButNotPresent() throws Exception {
        final String ruleNodeType = "nt:unstructured";
        final Map<String,String> nodePropRestrictions = new HashMap<String,String>();
        final Map<String,String> childPropRestrictions = new HashMap<String,String>();
        final String childType = "nt:unstructured";
        final boolean childRequired = true;
        final Outcome.Level severity = Outcome.Level.ERROR;

        // create rule in repo
        final Number minValue = 1;
        final Number maxValue = 3;
        final Rule rule = RuleFactory.createChildCountValidationRule( getTransaction(),
                                                                      _repo,
                                                                      this.name.getMethodName(),
                                                                      ruleNodeType,
                                                                      nodePropRestrictions,
                                                                      childType,
                                                                      childPropRestrictions,
                                                                      childRequired,
                                                                      minValue,
                                                                      true,
                                                                      maxValue,
                                                                      true,
                                                                      severity,
                                                                      _description,
                                                                      Collections.emptyList() );

        // setup KomodoObject
        final KomodoObject kobject = _repo.add( getTransaction(), null, "kobject", null );

        commit();

        // test
        final Result result = rule.evaluate( getTransaction(), kobject );
        assertThat( result.getLevel(), is( Level.ERROR ) );
        assertThat( result.getPath(), is( kobject.getAbsolutePath() ) );
        assertThat( result.getRuleId(), is( rule.getName( getTransaction() ) ) );
        assertThat( result.getMessage().contains(RULE_DESCRIPTION), is( true ) );
    }

    @Test
    public void shouldVerifySameNameSiblingRulePasses() throws Exception {
        final String ruleNodeType = "nt:unstructured";
        final String childType = "nt:unstructured";
        final boolean childRequired = true;
        final Map<String,String> nodePropRestrictions = new HashMap<String,String>();
        final Map<String,String> childPropRestrictions = new HashMap<String,String>();
        final Outcome.Level severity = Outcome.Level.ERROR;

        // create rule in repo
        final Rule rule = RuleFactory.createSameNameSiblingValidationRule(getTransaction(),
                                                                          _repo,
                                                                          this.name.getMethodName(),
                                                                          ruleNodeType,
                                                                          nodePropRestrictions,
                                                                          childType,
                                                                          childPropRestrictions,
                                                                          childRequired,
                                                                          false,
                                                                          severity,
                                                                          _description,
                                                                          Collections.emptyList());

        // setup KomodoObject
        final KomodoObject kobject = _repo.add( getTransaction(), null, "kobject", null );

        // adds two children with different names
        for ( int i = 0; i < 2; ++i ) {
            kobject.addChild( getTransaction(), ( "child" + i ), "nt:unstructured" );
        }

        commit();

        // test
        final Result result = rule.evaluate( getTransaction(), kobject );
        assertThat( result.isOK(), is( true ) );
        assertThat( result.getPath(), is( kobject.getAbsolutePath() ) );
        assertThat( result.getRuleId(), is( rule.getName( getTransaction() ) ) );
    }

    @Test
    public void shouldVerifySameNameSiblingRuleFails() throws Exception {
        final String ruleNodeType = "nt:unstructured";
        final String childType = "nt:unstructured";
        final boolean childRequired = true;
        final Map<String,String> nodePropRestrictions = new HashMap<String,String>();
        final Map<String,String> childPropRestrictions = new HashMap<String,String>();
        final Outcome.Level severity = Outcome.Level.ERROR;

        // create rule in repo
        final Rule rule = RuleFactory.createSameNameSiblingValidationRule(getTransaction(),
                                                                          _repo,
                                                                          this.name.getMethodName(),
                                                                          ruleNodeType,
                                                                          nodePropRestrictions,
                                                                          childType,
                                                                          childPropRestrictions,
                                                                          childRequired,
                                                                          false,
                                                                          severity,
                                                                          _description,
                                                                          Collections.emptyList());

        // setup KomodoObject
        final KomodoObject kobject = _repo.add( getTransaction(), null, "kobject", null );

        // adds two children with same name
        for ( int i = 0; i < 2; ++i ) {
            kobject.addChild( getTransaction(), ( "child" ), "nt:unstructured" );
        }

        commit();

        // test
        final Result result = rule.evaluate( getTransaction(), kobject );
        assertThat( result.isOK(), is( false ) );
        assertThat( result.getPath(), is( kobject.getAbsolutePath() ) );
        assertThat( result.getRuleId(), is( rule.getName( getTransaction() ) ) );
        assertThat( result.getMessage().contains(RULE_DESCRIPTION), is( true ) );
    }

    @Test
    public void shouldVerifyDisabledRulesCannotBeEvaluated() throws Exception {
        final String ruleNodeType = "nt:unstructured";
        final String propName = "sledge";
        final boolean propRequired = false;
        final Map<String,String> nodePropRestrictions = new HashMap<String,String>();
        final Outcome.Level severity = Outcome.Level.ERROR;

        // create rule in repo
        final String pattern = "[0-9]{3}-[0-9]{2}-[0-9]{4}";
        final Rule rule = RuleFactory.createPropertyPatternRule( getTransaction(),
                                                                 _repo,
                                                                 this.name.getMethodName(),
                                                                 ruleNodeType,
                                                                 nodePropRestrictions,
                                                                 propName,
                                                                 propRequired,
                                                                 pattern,
                                                                 severity,
                                                                 _description,
                                                                 Collections.emptyList() );

        // setup KomodoObject
        final KomodoObject kobject = _repo.add( getTransaction(), null, "kobject", null );
        kobject.setProperty( getTransaction(), propName, "elvis" );
        commit();

        // disable rule
        rule.setEnabled( getTransaction(), false );

        // test
        Result result = rule.evaluate( getTransaction(), kobject );

        assertThat( result.isOK(), is( false ) );
        assertThat( result.getPath(), is( kobject.getAbsolutePath() ) );
        assertThat( result.getRuleId(), is( rule.getName( getTransaction() ) ) );
        assertThat( result.getLevel(), is(Outcome.Level.INFO));
        assertThat( result.getMessage().contains("\""+this.name.getMethodName()+"\" is disabled."), is( true ) );
    }

    @Test
    public void shouldVerifyNodeNameRulePassesWhenNameMatchesPattern() throws Exception {
        final String ruleNodeType = "nt:unstructured";
        final Map<String,String> nodePropRestrictions = new HashMap<String,String>();
        final Outcome.Level severity = Outcome.Level.ERROR;
        final String pattern = "[A-Z]{3}";
        final Rule rule = RuleFactory.createNodeNameRule( getTransaction(),
                                                          _repo,
                                                          this.name.getMethodName(),
                                                          ruleNodeType,
                                                          nodePropRestrictions,
                                                          pattern,
                                                          severity,
                                                          _description,
                                                          Collections.emptyList() );

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
    public void shouldVerifyNodeNameRuleFailsWhenNameDoesNotMatchesPattern() throws Exception {
        final String ruleNodeType = "nt:unstructured";
        final Map<String,String> nodePropRestrictions = new HashMap<String,String>();
        final Outcome.Level severity = Outcome.Level.ERROR;
        final String pattern = "[A-Z]{3}";
        final Rule rule = RuleFactory.createNodeNameRule( getTransaction(),
                                                          _repo,
                                                          this.name.getMethodName(),
                                                          ruleNodeType,
                                                          nodePropRestrictions,
                                                          pattern,
                                                          severity,
                                                          _description,
                                                          Collections.emptyList() );

        // setup KomodoObject
        final KomodoObject kobject = _repo.add( getTransaction(), null, "ABc", null );
        commit();

        // test
        final Result result = rule.evaluate( getTransaction(), kobject );
        assertThat( result.getLevel(), is( Level.ERROR ) );
        assertThat( result.getPath(), is( kobject.getAbsolutePath() ) );
        assertThat( result.getRuleId(), is( rule.getName( getTransaction() ) ) );
        assertThat( result.getMessage().contains(RULE_DESCRIPTION), is( true ) );
    }

    @Test
    public void shouldVerifyPropertyRangeRulePassesWhenValueWithinRange() throws Exception {
        final String ruleNodeType = "nt:unstructured";
        final String propName = "sledge";
        final boolean propRequired = true;
        final Map<String,String> nodePropRestrictions = new HashMap<String,String>();
        final Outcome.Level severity = Outcome.Level.ERROR;

        // create rule in repo
        final Number minValue = 5;
        final Number maxValue = 10;
        final Rule rule = RuleFactory.createPropertyValueNumberValidationRule( getTransaction(),
                                                                               _repo,
                                                                               this.name.getMethodName(),
                                                                               ruleNodeType,
                                                                               nodePropRestrictions,
                                                                               propName,
                                                                               propRequired,
                                                                               minValue,
                                                                               true,
                                                                               maxValue,
                                                                               true,
                                                                               severity,
                                                                               _description,
                                                                               Collections.emptyList() );

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
    public void shouldVerifyPropertyRangeRuleFailsWhenPropRequiredButNotPresent() throws Exception {
        final String ruleNodeType = "nt:unstructured";
        final String propName = "sledge";
        final boolean propRequired = true;
        final Map<String,String> nodePropRestrictions = new HashMap<String,String>();
        final Outcome.Level severity = Outcome.Level.ERROR;

        // create rule in repo
        final Number minValue = 5;
        final Number maxValue = 10;
        final Rule rule = RuleFactory.createPropertyValueNumberValidationRule( getTransaction(),
                                                                               _repo,
                                                                               this.name.getMethodName(),
                                                                               ruleNodeType,
                                                                               nodePropRestrictions,
                                                                               propName,
                                                                               propRequired,
                                                                               minValue,
                                                                               true,
                                                                               maxValue,
                                                                               true,
                                                                               severity,
                                                                               _description,
                                                                               Collections.emptyList() );

        // setup KomodoObject
        final KomodoObject kobject = _repo.add( getTransaction(), null, "kobject", null );
        commit();

        // test
        final Result result = rule.evaluate( getTransaction(), kobject );
        assertThat( result.isOK(), is( false ) );
        assertThat( result.getPath(), is( kobject.getAbsolutePath() ) );
        assertThat( result.getRuleId(), is( rule.getName( getTransaction() ) ) );
        assertThat( result.getMessage().contains(RULE_DESCRIPTION), is( true ) );
    }

    @Test
    public void shouldVerifyPropertyRangeRuleFailsWhenValueEqualsMaxButMaxNotInclusive() throws Exception {
        final String ruleNodeType = "nt:unstructured";
        final String propName = "sledge";
        final boolean propRequired = true;
        final Map<String,String> nodePropRestrictions = new HashMap<String,String>();
        final Outcome.Level severity = Outcome.Level.ERROR;

        // create rule in repo
        final Number minValue = 5;
        final Number maxValue = 10;
        final Rule rule = RuleFactory.createPropertyValueNumberValidationRule( getTransaction(),
                                                                               _repo,
                                                                               this.name.getMethodName(),
                                                                               ruleNodeType,
                                                                               nodePropRestrictions,
                                                                               propName,
                                                                               propRequired,
                                                                               minValue,
                                                                               true,
                                                                               maxValue,
                                                                               false,
                                                                               severity,
                                                                               _description,
                                                                               Collections.emptyList() );

        // setup KomodoObject
        final KomodoObject kobject = _repo.add( getTransaction(), null, "kobject", null );
        kobject.setProperty( getTransaction(), propName, maxValue );
        commit();

        // test
        final Result result = rule.evaluate( getTransaction(), kobject );
        assertThat( result.getLevel(), is( Level.ERROR ) );
        assertThat( result.getPath(), is( kobject.getAbsolutePath() ) );
        assertThat( result.getRuleId(), is( rule.getName( getTransaction() ) ) );
        assertThat( result.getMessage().contains(RULE_DESCRIPTION), is( true ) );
    }

    @Test
    public void shouldVerifyPropertyRangeRuleFailsWhenValueEqualsMinButMinNotInclusive() throws Exception {
        final String ruleNodeType = "nt:unstructured";
        final String propName = "sledge";
        final boolean propRequired = true;
        final Map<String,String> nodePropRestrictions = new HashMap<String,String>();
        final Outcome.Level severity = Outcome.Level.ERROR;

        // create rule in repo
        final Number minValue = 5;
        final Number maxValue = 10;
        final Rule rule = RuleFactory.createPropertyValueNumberValidationRule( getTransaction(),
                                                                               _repo,
                                                                               this.name.getMethodName(),
                                                                               ruleNodeType,
                                                                               nodePropRestrictions,
                                                                               propName,
                                                                               propRequired,
                                                                               minValue,
                                                                               false,
                                                                               maxValue,
                                                                               true,
                                                                               severity,
                                                                               _description,
                                                                               Collections.emptyList() );

        // setup KomodoObject
        final KomodoObject kobject = _repo.add( getTransaction(), null, "kobject", null );
        kobject.setProperty( getTransaction(), propName, minValue );
        commit();

        // test
        final Result result = rule.evaluate( getTransaction(), kobject );
        assertThat( result.getLevel(), is( Level.ERROR ) );
        assertThat( result.getPath(), is( kobject.getAbsolutePath() ) );
        assertThat( result.getRuleId(), is( rule.getName( getTransaction() ) ) );
        assertThat( result.getMessage().contains(RULE_DESCRIPTION), is( true ) );
    }

    @Test
    public void shouldVerifyPropertyRelationshipRulePasses() throws Exception {
        final String ruleNodeType = "nt:unstructured";
        final String propName = "sledge";
        final boolean propRequired = true;
        final Map<String,String> nodePropRestrictions = new HashMap<String,String>();
        final Outcome.Level severity = Outcome.Level.ERROR;
        final String[] propsExists = new String[] { "tko:text", "tko:type" };
        final String[] propsAbsent = new String[] { "tko:jcrName", "tko:ruleType" };
        final String[] childExists = new String[] { "tko:schemas", "tko:dataSources" };
        final String[] childAbsent = new String[] { "tko:vdbModels", "tko:vdbEntries" };

        // create rule in repo
        final Rule rule = RuleFactory.createPropertyRelationshipValidationRule( getTransaction(),
                                                                                _repo,
                                                                                this.name.getMethodName(),
                                                                                ruleNodeType,
                                                                                nodePropRestrictions,
                                                                                propName,
                                                                                propRequired,
                                                                                Arrays.asList( propsExists ),
                                                                                Arrays.asList( propsAbsent ),
                                                                                Arrays.asList( childExists ),
                                                                                Arrays.asList( childAbsent ),
                                                                                severity,
                                                                                _description,
                                                                                Collections.emptyList() );

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
    public void shouldVerifyPropertyRelationshipRuleFailsWhenChildDoesNotExist() throws Exception {
        final String ruleNodeType = "nt:unstructured";
        final String propName = "sledge";
        final boolean propRequired = true;
        final Map<String,String> nodePropRestrictions = new HashMap<String,String>();
        final Outcome.Level severity = Outcome.Level.ERROR;
        final String[] propsExists = new String[] { "tko:text", "tko:type" };
        final String[] propsAbsent = new String[] { "tko:jcrName", "tko:ruleType" };
        final String[] childExists = new String[] { "tko:schemas", "tko:dataSources" };
        final String[] childAbsent = new String[] { "tko:vdbModels", "tko:vdbEntries" };

        // create rule in repo
        final Rule rule = RuleFactory.createPropertyRelationshipValidationRule( getTransaction(),
                                                                                _repo,
                                                                                this.name.getMethodName(),
                                                                                ruleNodeType,
                                                                                nodePropRestrictions,
                                                                                propName,
                                                                                propRequired,
                                                                                Arrays.asList( propsExists ),
                                                                                Arrays.asList( propsAbsent ),
                                                                                Arrays.asList( childExists ),
                                                                                Arrays.asList( childAbsent ),
                                                                                severity,
                                                                                _description,
                                                                                Collections.emptyList() );

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
        assertThat( result.getMessage().contains(RULE_DESCRIPTION), is( true ) );
    }

    @Test
    public void shouldVerifyPropertyRelationshipRuleFailsWhenChildShouldNotExist() throws Exception {
        final String ruleNodeType = "nt:unstructured";
        final String propName = "sledge";
        final boolean propRequired = true;
        final Map<String,String> nodePropRestrictions = new HashMap<String,String>();
        final Outcome.Level severity = Outcome.Level.ERROR;
        final String[] propsExists = new String[] { "tko:text", "tko:type" };
        final String[] propsAbsent = new String[] { "tko:jcrName", "tko:ruleType" };
        final String[] childExists = new String[] { "tko:schemas" };
        final String[] childAbsent = new String[] { "tko:vdbModels", "tko:vdbEntries", "tko:dataSources" };

        // create rule in repo
        final Rule rule = RuleFactory.createPropertyRelationshipValidationRule( getTransaction(),
                                                                                _repo,
                                                                                this.name.getMethodName(),
                                                                                ruleNodeType,
                                                                                nodePropRestrictions,
                                                                                propName,
                                                                                propRequired,
                                                                                Arrays.asList( propsExists ),
                                                                                Arrays.asList( propsAbsent ),
                                                                                Arrays.asList( childExists ),
                                                                                Arrays.asList( childAbsent ),
                                                                                severity,
                                                                                _description,
                                                                                Collections.emptyList() );

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
        assertThat( result.getMessage().contains(RULE_DESCRIPTION), is( true ) );
    }

    @Test
    public void shouldVerifyPropertyRelationshipRuleFailsWhenPropertyDoesNotExist() throws Exception {
        final String ruleNodeType = "nt:unstructured";
        final String propName = "sledge";
        final boolean propRequired = true;
        final Map<String,String> nodePropRestrictions = new HashMap<String,String>();
        final Outcome.Level severity = Outcome.Level.ERROR;
        final String[] propsExists = new String[] { "tko:text", "tko:type" };
        final String[] propsAbsent = new String[] { "tko:jcrName", "tko:ruleType" };
        final String[] childExists = new String[] { "tko:schemas", "tko:dataSources" };
        final String[] childAbsent = new String[] { "tko:vdbModels", "tko:vdbEntries" };

        // create rule in repo
        final Rule rule = RuleFactory.createPropertyRelationshipValidationRule( getTransaction(),
                                                                                _repo,
                                                                                this.name.getMethodName(),
                                                                                ruleNodeType,
                                                                                nodePropRestrictions,
                                                                                propName,
                                                                                propRequired,
                                                                                Arrays.asList( propsExists ),
                                                                                Arrays.asList( propsAbsent ),
                                                                                Arrays.asList( childExists ),
                                                                                Arrays.asList( childAbsent ),
                                                                                severity,
                                                                                _description,
                                                                                Collections.emptyList() );

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
        assertThat( result.getMessage().contains(RULE_DESCRIPTION), is( true ) );
    }

    @Test
    public void shouldVerifyPropertyRelationshipRuleFailsWhenPropertyShouldNotExist() throws Exception {
        final String ruleNodeType = "nt:unstructured";
        final String propName = "sledge";
        final boolean propRequired = true;
        final Map<String,String> nodePropRestrictions = new HashMap<String,String>();
        final Outcome.Level severity = Outcome.Level.ERROR;
        final String[] propsExists = new String[] { "tko:text" };
        final String[] propsAbsent = new String[] { "tko:jcrName", "tko:ruleType", "tko:type" };
        final String[] childExists = new String[] { "tko:schemas", "tko:dataSources" };
        final String[] childAbsent = new String[] { "tko:vdbModels", "tko:vdbEntries" };

        // create rule in repo
        final Rule rule = RuleFactory.createPropertyRelationshipValidationRule( getTransaction(),
                                                                                _repo,
                                                                                this.name.getMethodName(),
                                                                                ruleNodeType,
                                                                                nodePropRestrictions,
                                                                                propName,
                                                                                propRequired,
                                                                                Arrays.asList( propsExists ),
                                                                                Arrays.asList( propsAbsent ),
                                                                                Arrays.asList( childExists ),
                                                                                Arrays.asList( childAbsent ),
                                                                                severity,
                                                                                _description,
                                                                                Collections.emptyList() );

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
        assertThat( result.getMessage().contains(RULE_DESCRIPTION), is( true ) );
    }

    @Test
    public void shouldVerifyPropertyValueRulePassesWhenValueMatchesPattern() throws Exception {
        final String ruleNodeType = "nt:unstructured";
        final String propName = "sledge";
        final boolean propRequired = false;
        final Map<String,String> nodePropRestrictions = new HashMap<String,String>();
        final Outcome.Level severity = Outcome.Level.ERROR;

        // create rule in repo
        final String pattern = "[0-9]{3}-[0-9]{2}-[0-9]{4}";
        final Rule rule = RuleFactory.createPropertyPatternRule( getTransaction(),
                                                                 _repo,
                                                                 this.name.getMethodName(),
                                                                 ruleNodeType,
                                                                 nodePropRestrictions,
                                                                 propName,
                                                                 propRequired,
                                                                 pattern,
                                                                 severity,
                                                                 _description,
                                                                 Collections.emptyList() );

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
    public void shouldVerifyPropertyValueRuleFailsWhenPropRequiredButNotPresent() throws Exception {
        final String ruleNodeType = "nt:unstructured";
        final String propName = "sledge";
        final boolean propRequired = true;
        final Map<String,String> nodePropRestrictions = new HashMap<String,String>();
        final Outcome.Level severity = Outcome.Level.ERROR;

        // create rule in repo
        final String pattern = "[0-9]{3}-[0-9]{2}-[0-9]{4}";
        final Rule rule = RuleFactory.createPropertyPatternRule( getTransaction(),
                                                                 _repo,
                                                                 this.name.getMethodName(),
                                                                 ruleNodeType,
                                                                 nodePropRestrictions,
                                                                 propName,
                                                                 propRequired,
                                                                 pattern,
                                                                 severity,
                                                                 _description,
                                                                 Collections.emptyList() );

        // setup KomodoObject
        final KomodoObject kobject = _repo.add( getTransaction(), null, "kobject", null );
        commit();

        // test
        final Result result = rule.evaluate( getTransaction(), kobject );
        assertThat( result.getLevel(), is( Level.ERROR ) );
        assertThat( result.getPath(), is( kobject.getAbsolutePath() ) );
        assertThat( result.getRuleId(), is( rule.getName( getTransaction() ) ) );
        assertThat( result.getMessage().contains(RULE_DESCRIPTION), is( true ) );
    }

    @Test
    public void shouldVerifyPropertyValueRuleFailsWhenValueDoesNotMatchPattern() throws Exception {
        final String ruleNodeType = "nt:unstructured";
        final String propName = "sledge";
        final boolean propRequired = false;
        final Map<String,String> nodePropRestrictions = new HashMap<String,String>();
        final Outcome.Level severity = Outcome.Level.ERROR;

        // create rule in repo
        final String pattern = "[0-9]{3}-[0-9]{2}-[0-9]{4}";
        final Rule rule = RuleFactory.createPropertyPatternRule( getTransaction(),
                                                                 _repo,
                                                                 this.name.getMethodName(),
                                                                 ruleNodeType,
                                                                 nodePropRestrictions,
                                                                 propName,
                                                                 propRequired,
                                                                 pattern,
                                                                 severity,
                                                                 _description,
                                                                 Collections.emptyList() );

        // setup KomodoObject
        final KomodoObject kobject = _repo.add( getTransaction(), null, "kobject", null );
        kobject.setProperty( getTransaction(), propName, "elvis" );
        commit();

        // test
        final Result result = rule.evaluate( getTransaction(), kobject );
        assertThat( result.getLevel(), is( Level.ERROR ) );
        assertThat( result.getPath(), is( kobject.getAbsolutePath() ) );
        assertThat( result.getRuleId(), is( rule.getName( getTransaction() ) ) );
        assertThat( result.getMessage().contains(RULE_DESCRIPTION), is( true ) );
    }

    @Test
    public void shouldRuleWarningSeverityOnFailure() throws Exception {
        final String ruleNodeType = "nt:unstructured";
        final String propName = "sledge";
        final boolean propRequired = false;
        final Map<String,String> nodePropRestrictions = new HashMap<String,String>();
        final Outcome.Level severity = Outcome.Level.WARNING;

        // create rule in repo
        final String pattern = "[0-9]{3}-[0-9]{2}-[0-9]{4}";
        final Rule rule = RuleFactory.createPropertyPatternRule( getTransaction(),
                                                                 _repo,
                                                                 this.name.getMethodName(),
                                                                 ruleNodeType,
                                                                 nodePropRestrictions,
                                                                 propName,
                                                                 propRequired,
                                                                 pattern,
                                                                 severity,
                                                                 _description,
                                                                 Collections.emptyList() );

        // setup KomodoObject
        final KomodoObject kobject = _repo.add( getTransaction(), null, "kobject", null );
        kobject.setProperty( getTransaction(), propName, "elvis" );
        commit();

        // test
        final Result result = rule.evaluate( getTransaction(), kobject );
        assertThat( result.getLevel(), is( Level.WARNING ) );
        assertThat( result.getPath(), is( kobject.getAbsolutePath() ) );
        assertThat( result.getRuleId(), is( rule.getName( getTransaction() ) ) );
        assertThat( result.getMessage().contains(RULE_DESCRIPTION), is( true ) );
    }

    @Test
    public void shouldRuleInfoSeverityOnFailure() throws Exception {
        final String ruleNodeType = "nt:unstructured";
        final String propName = "sledge";
        final boolean propRequired = false;
        final Map<String,String> nodePropRestrictions = new HashMap<String,String>();
        final Outcome.Level severity = Outcome.Level.INFO;

        // create rule in repo
        final String pattern = "[0-9]{3}-[0-9]{2}-[0-9]{4}";
        final Rule rule = RuleFactory.createPropertyPatternRule( getTransaction(),
                                                                 _repo,
                                                                 this.name.getMethodName(),
                                                                 ruleNodeType,
                                                                 nodePropRestrictions,
                                                                 propName,
                                                                 propRequired,
                                                                 pattern,
                                                                 severity,
                                                                 _description,
                                                                 Collections.emptyList() );

        // setup KomodoObject
        final KomodoObject kobject = _repo.add( getTransaction(), null, "kobject", null );
        kobject.setProperty( getTransaction(), propName, "elvis" );
        commit();

        // test
        final Result result = rule.evaluate( getTransaction(), kobject );
        assertThat( result.getLevel(), is( Level.INFO ) );
        assertThat( result.getPath(), is( kobject.getAbsolutePath() ) );
        assertThat( result.getRuleId(), is( rule.getName( getTransaction() ) ) );
        assertThat( result.getMessage().contains(RULE_DESCRIPTION), is( true ) );
    }

    @Test
    public void shouldVerifyGetDescriptionOnFailure() throws Exception {
        // If no messages are supplied in the rule definition,
        // the rule failure message should be the rule 'description'

        final String ruleNodeType = "nt:unstructured";
        final String propName = "sledge";
        final boolean propRequired = false;
        final Map<String,String> nodePropRestrictions = new HashMap<String,String>();
        final Outcome.Level severity = Outcome.Level.ERROR;

        // create rule in repo
        final String pattern = "[0-9]{3}-[0-9]{2}-[0-9]{4}";
        final Rule rule = RuleFactory.createPropertyPatternRule( getTransaction(),
                                                                 _repo,
                                                                 this.name.getMethodName(),
                                                                 ruleNodeType,
                                                                 nodePropRestrictions,
                                                                 propName,
                                                                 propRequired,
                                                                 pattern,
                                                                 severity,
                                                                 _description,
                                                                 Collections.emptyList() );

        // setup KomodoObject
        final KomodoObject kobject = _repo.add( getTransaction(), null, "kobject", null );
        kobject.setProperty( getTransaction(), propName, "elvis" );
        commit();

        // test
        final Result result = rule.evaluate( getTransaction(), kobject );
        assertThat( result.getLevel(), is( Level.ERROR ) );
        assertThat( result.getPath(), is( kobject.getAbsolutePath() ) );
        assertThat( result.getRuleId(), is( rule.getName( getTransaction() ) ) );
        assertThat( result.getMessage().contains(RULE_DESCRIPTION), is( true ) );
    }

    @Test
    public void shouldVerifyGetUserOverrideMessageOnFailure() throws Exception {
        // If message is supplied in the rule definition, the rule failure message
        // should be the 'message' - it overrides the 'description'
        String messageOverride = "This is a rule error message";
        _message = Collections.singletonList( new LocalizedMessage( MessageKey.PATTERN_RULE_INVALID_PROPERTY_VALUE.name(), "en", messageOverride ) );

        final String ruleNodeType = "nt:unstructured";
        final String propName = "sledge";
        final boolean propRequired = false;
        final Map<String,String> nodePropRestrictions = new HashMap<String,String>();
        final Outcome.Level severity = Outcome.Level.ERROR;

        // create rule in repo
        final String pattern = "[0-9]{3}-[0-9]{2}-[0-9]{4}";
        final Rule rule = RuleFactory.createPropertyPatternRule( getTransaction(),
                                                                 _repo,
                                                                 this.name.getMethodName(),
                                                                 ruleNodeType,
                                                                 nodePropRestrictions,
                                                                 propName,
                                                                 propRequired,
                                                                 pattern,
                                                                 severity,
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
        assertThat( result.getMessage().contains(messageOverride), is( true ) );
    }

}
