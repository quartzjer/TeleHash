/**
 * <copyright>
 * </copyright>
 *
 * $Id$
 */
package org.telehash.model.impl;

import java.net.InetSocketAddress;

import org.apache.mina.core.session.IoSession;
import org.eclipse.emf.ecore.EAttribute;
import org.eclipse.emf.ecore.EClass;
import org.eclipse.emf.ecore.EDataType;
import org.eclipse.emf.ecore.EOperation;
import org.eclipse.emf.ecore.EPackage;
import org.eclipse.emf.ecore.EReference;
import org.eclipse.emf.ecore.impl.EPackageImpl;
import org.eclipse.emf.json.model.JsonPackage;
import org.telehash.Hash;
import org.telehash.model.Line;
import org.telehash.model.TapRule;
import org.telehash.model.TelehashFactory;
import org.telehash.model.TelehashPackage;
import org.telehash.model.Telex;

/**
 * <!-- begin-user-doc -->
 * An implementation of the model <b>Package</b>.
 * <!-- end-user-doc -->
 * @generated
 */
public class TelehashPackageImpl extends EPackageImpl implements
		TelehashPackage {
	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	private EClass telexEClass = null;

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	private EClass tapRuleEClass = null;

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	private EClass lineEClass = null;

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	private EDataType endpointEDataType = null;

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	private EDataType hashEDataType = null;

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	private EDataType ioSessionEDataType = null;

	/**
	 * Creates an instance of the model <b>Package</b>, registered with
	 * {@link org.eclipse.emf.ecore.EPackage.Registry EPackage.Registry} by the package
	 * package URI value.
	 * <p>Note: the correct way to create the package is via the static
	 * factory method {@link #init init()}, which also performs
	 * initialization of the package, or returns the registered package,
	 * if one already exists.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see org.eclipse.emf.ecore.EPackage.Registry
	 * @see org.telehash.model.TelehashPackage#eNS_URI
	 * @see #init()
	 * @generated
	 */
	private TelehashPackageImpl() {
		super(eNS_URI, TelehashFactory.eINSTANCE);
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	private static boolean isInited = false;

	/**
	 * Creates, registers, and initializes the <b>Package</b> for this model, and for any others upon which it depends.
	 * 
	 * <p>This method is used to initialize {@link TelehashPackage#eINSTANCE} when that field is accessed.
	 * Clients should not invoke it directly. Instead, they should simply access that field to obtain the package.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #eNS_URI
	 * @see #createPackageContents()
	 * @see #initializePackageContents()
	 * @generated
	 */
	public static TelehashPackage init() {
		if (isInited)
			return (TelehashPackage) EPackage.Registry.INSTANCE
					.getEPackage(TelehashPackage.eNS_URI);

		// Obtain or create and register package
		TelehashPackageImpl theTelehashPackage = (TelehashPackageImpl) (EPackage.Registry.INSTANCE
				.get(eNS_URI) instanceof TelehashPackageImpl ? EPackage.Registry.INSTANCE
				.get(eNS_URI) : new TelehashPackageImpl());

		isInited = true;

		// Initialize simple dependencies
		JsonPackage.eINSTANCE.eClass();

		// Create package meta-data objects
		theTelehashPackage.createPackageContents();

		// Initialize created meta-data
		theTelehashPackage.initializePackageContents();

		// Mark meta-data to indicate it can't be changed
		theTelehashPackage.freeze();

		// Update the registry and return the package
		EPackage.Registry.INSTANCE.put(TelehashPackage.eNS_URI,
				theTelehashPackage);
		return theTelehashPackage;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public EClass getTelex() {
		return telexEClass;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public EAttribute getTelex_To() {
		return (EAttribute) telexEClass.getEStructuralFeatures().get(0);
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public EAttribute getTelex_End() {
		return (EAttribute) telexEClass.getEStructuralFeatures().get(1);
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public EAttribute getTelex_Line() {
		return (EAttribute) telexEClass.getEStructuralFeatures().get(2);
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public EAttribute getTelex_Ring() {
		return (EAttribute) telexEClass.getEStructuralFeatures().get(3);
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public EAttribute getTelex_See() {
		return (EAttribute) telexEClass.getEStructuralFeatures().get(5);
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public EReference getTelex_Tap() {
		return (EReference) telexEClass.getEStructuralFeatures().get(6);
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public EAttribute getTelex_BytesReceived() {
		return (EAttribute) telexEClass.getEStructuralFeatures().get(4);
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public EClass getTapRule() {
		return tapRuleEClass;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public EReference getTapRule_Is() {
		return (EReference) tapRuleEClass.getEStructuralFeatures().get(0);
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public EAttribute getTapRule_Has() {
		return (EAttribute) tapRuleEClass.getEStructuralFeatures().get(1);
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public EClass getLine() {
		return lineEClass;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public EAttribute getLine_Address() {
		return (EAttribute) lineEClass.getEStructuralFeatures().get(0);
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public EAttribute getLine_End() {
		return (EAttribute) lineEClass.getEStructuralFeatures().get(1);
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public EAttribute getLine_Neighbors() {
		return (EAttribute) lineEClass.getEStructuralFeatures().get(2);
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public EAttribute getLine_RingIn() {
		return (EAttribute) lineEClass.getEStructuralFeatures().get(3);
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public EAttribute getLine_RingOut() {
		return (EAttribute) lineEClass.getEStructuralFeatures().get(4);
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public EAttribute getLine_Init() {
		return (EAttribute) lineEClass.getEStructuralFeatures().get(5);
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public EAttribute getLine_SeenAt() {
		return (EAttribute) lineEClass.getEStructuralFeatures().get(6);
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public EAttribute getLine_SentAt() {
		return (EAttribute) lineEClass.getEStructuralFeatures().get(7);
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public EAttribute getLine_LineAt() {
		return (EAttribute) lineEClass.getEStructuralFeatures().get(8);
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public EAttribute getLine_TapLastAt() {
		return (EAttribute) lineEClass.getEStructuralFeatures().get(9);
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public EAttribute getLine_Br() {
		return (EAttribute) lineEClass.getEStructuralFeatures().get(10);
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public EAttribute getLine_BrIn() {
		return (EAttribute) lineEClass.getEStructuralFeatures().get(11);
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public EAttribute getLine_BrOut() {
		return (EAttribute) lineEClass.getEStructuralFeatures().get(12);
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public EAttribute getLine_Bsent() {
		return (EAttribute) lineEClass.getEStructuralFeatures().get(13);
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public EAttribute getLine_LineId() {
		return (EAttribute) lineEClass.getEStructuralFeatures().get(14);
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public EAttribute getLine_Visible() {
		return (EAttribute) lineEClass.getEStructuralFeatures().get(15);
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public EAttribute getLine_Advertised() {
		return (EAttribute) lineEClass.getEStructuralFeatures().get(16);
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public EReference getLine_Rules() {
		return (EReference) lineEClass.getEStructuralFeatures().get(17);
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public EAttribute getLine_Session() {
		return (EAttribute) lineEClass.getEStructuralFeatures().get(18);
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public EDataType getEndpoint() {
		return endpointEDataType;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public EDataType getHash() {
		return hashEDataType;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public EDataType getIoSession() {
		return ioSessionEDataType;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public TelehashFactory getTelehashFactory() {
		return (TelehashFactory) getEFactoryInstance();
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	private boolean isCreated = false;

	/**
	 * Creates the meta-model objects for the package.  This method is
	 * guarded to have no affect on any invocation but its first.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public void createPackageContents() {
		if (isCreated)
			return;
		isCreated = true;

		// Create classes and their features
		telexEClass = createEClass(TELEX);
		createEAttribute(telexEClass, TELEX__TO);
		createEAttribute(telexEClass, TELEX__END);
		createEAttribute(telexEClass, TELEX__LINE);
		createEAttribute(telexEClass, TELEX__RING);
		createEAttribute(telexEClass, TELEX__BYTES_RECEIVED);
		createEAttribute(telexEClass, TELEX__SEE);
		createEReference(telexEClass, TELEX__TAP);

		tapRuleEClass = createEClass(TAP_RULE);
		createEReference(tapRuleEClass, TAP_RULE__IS);
		createEAttribute(tapRuleEClass, TAP_RULE__HAS);

		lineEClass = createEClass(LINE);
		createEAttribute(lineEClass, LINE__ADDRESS);
		createEAttribute(lineEClass, LINE__END);
		createEAttribute(lineEClass, LINE__NEIGHBORS);
		createEAttribute(lineEClass, LINE__RING_IN);
		createEAttribute(lineEClass, LINE__RING_OUT);
		createEAttribute(lineEClass, LINE__INIT);
		createEAttribute(lineEClass, LINE__SEEN_AT);
		createEAttribute(lineEClass, LINE__SENT_AT);
		createEAttribute(lineEClass, LINE__LINE_AT);
		createEAttribute(lineEClass, LINE__TAP_LAST_AT);
		createEAttribute(lineEClass, LINE__BR);
		createEAttribute(lineEClass, LINE__BR_IN);
		createEAttribute(lineEClass, LINE__BR_OUT);
		createEAttribute(lineEClass, LINE__BSENT);
		createEAttribute(lineEClass, LINE__LINE_ID);
		createEAttribute(lineEClass, LINE__VISIBLE);
		createEAttribute(lineEClass, LINE__ADVERTISED);
		createEReference(lineEClass, LINE__RULES);
		createEAttribute(lineEClass, LINE__SESSION);

		// Create data types
		endpointEDataType = createEDataType(ENDPOINT);
		hashEDataType = createEDataType(HASH);
		ioSessionEDataType = createEDataType(IO_SESSION);
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	private boolean isInitialized = false;

	/**
	 * Complete the initialization of the package and its meta-model.  This
	 * method is guarded to have no affect on any invocation but its first.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public void initializePackageContents() {
		if (isInitialized)
			return;
		isInitialized = true;

		// Initialize package
		setName(eNAME);
		setNsPrefix(eNS_PREFIX);
		setNsURI(eNS_URI);

		// Obtain other dependent packages
		JsonPackage theJsonPackage = (JsonPackage) EPackage.Registry.INSTANCE
				.getEPackage(JsonPackage.eNS_URI);

		// Create type parameters

		// Set bounds for type parameters

		// Add supertypes to classes
		telexEClass.getESuperTypes().add(theJsonPackage.getJsObject());
		tapRuleEClass.getESuperTypes().add(theJsonPackage.getJsObject());

		// Initialize classes and features; add operations and parameters
		initEClass(telexEClass, Telex.class, "Telex", !IS_ABSTRACT,
				!IS_INTERFACE, IS_GENERATED_INSTANCE_CLASS);
		initEAttribute(getTelex_To(), this.getEndpoint(), "to", null, 0, 1,
				Telex.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE,
				!IS_UNSETTABLE, !IS_ID, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);
		initEAttribute(getTelex_End(), this.getHash(), "end", null, 0, 1,
				Telex.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE,
				IS_UNSETTABLE, !IS_ID, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);
		initEAttribute(getTelex_Line(), ecorePackage.getEInt(), "line", null,
				0, 1, Telex.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE,
				IS_UNSETTABLE, !IS_ID, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);
		initEAttribute(getTelex_Ring(), ecorePackage.getEInt(), "ring", null,
				0, 1, Telex.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE,
				IS_UNSETTABLE, !IS_ID, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);
		initEAttribute(getTelex_BytesReceived(), ecorePackage.getEInt(),
				"bytesReceived", "0", 0, 1, Telex.class, !IS_TRANSIENT,
				!IS_VOLATILE, IS_CHANGEABLE, IS_UNSETTABLE, !IS_ID, IS_UNIQUE,
				!IS_DERIVED, IS_ORDERED);
		initEAttribute(getTelex_See(), this.getEndpoint(), "see", null, 0, -1,
				Telex.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE,
				IS_UNSETTABLE, !IS_ID, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);
		initEReference(getTelex_Tap(), this.getTapRule(), null, "tap", null, 0,
				-1, Telex.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE,
				IS_COMPOSITE, !IS_RESOLVE_PROXIES, IS_UNSETTABLE, IS_UNIQUE,
				!IS_DERIVED, IS_ORDERED);

		initEClass(tapRuleEClass, TapRule.class, "TapRule", !IS_ABSTRACT,
				!IS_INTERFACE, IS_GENERATED_INSTANCE_CLASS);
		initEReference(getTapRule_Is(), theJsonPackage.getJsObject(), null,
				"is", null, 0, 1, TapRule.class, !IS_TRANSIENT, !IS_VOLATILE,
				IS_CHANGEABLE, IS_COMPOSITE, !IS_RESOLVE_PROXIES,
				IS_UNSETTABLE, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);
		initEAttribute(getTapRule_Has(), ecorePackage.getEString(), "has",
				null, 0, -1, TapRule.class, !IS_TRANSIENT, !IS_VOLATILE,
				IS_CHANGEABLE, IS_UNSETTABLE, !IS_ID, IS_UNIQUE, !IS_DERIVED,
				IS_ORDERED);

		initEClass(lineEClass, Line.class, "Line", !IS_ABSTRACT, !IS_INTERFACE,
				IS_GENERATED_INSTANCE_CLASS);
		initEAttribute(getLine_Address(), this.getEndpoint(), "address", null,
				0, 1, Line.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE,
				!IS_UNSETTABLE, !IS_ID, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);
		initEAttribute(getLine_End(), this.getHash(), "end", null, 0, 1,
				Line.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE,
				!IS_UNSETTABLE, !IS_ID, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);
		initEAttribute(getLine_Neighbors(), this.getHash(), "neighbors", null,
				0, -1, Line.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE,
				!IS_UNSETTABLE, !IS_ID, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);
		initEAttribute(getLine_RingIn(), ecorePackage.getEInt(), "ringIn",
				null, 0, 1, Line.class, !IS_TRANSIENT, !IS_VOLATILE,
				IS_CHANGEABLE, IS_UNSETTABLE, !IS_ID, IS_UNIQUE, !IS_DERIVED,
				IS_ORDERED);
		initEAttribute(getLine_RingOut(), ecorePackage.getEInt(), "ringOut",
				null, 0, 1, Line.class, !IS_TRANSIENT, !IS_VOLATILE,
				IS_CHANGEABLE, IS_UNSETTABLE, !IS_ID, IS_UNIQUE, !IS_DERIVED,
				IS_ORDERED);
		initEAttribute(getLine_Init(), ecorePackage.getELong(), "init", null,
				0, 1, Line.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE,
				IS_UNSETTABLE, !IS_ID, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);
		initEAttribute(getLine_SeenAt(), ecorePackage.getELong(), "seenAt",
				null, 0, 1, Line.class, !IS_TRANSIENT, !IS_VOLATILE,
				IS_CHANGEABLE, IS_UNSETTABLE, !IS_ID, IS_UNIQUE, !IS_DERIVED,
				IS_ORDERED);
		initEAttribute(getLine_SentAt(), ecorePackage.getELong(), "sentAt",
				null, 0, 1, Line.class, !IS_TRANSIENT, !IS_VOLATILE,
				IS_CHANGEABLE, IS_UNSETTABLE, !IS_ID, IS_UNIQUE, !IS_DERIVED,
				IS_ORDERED);
		initEAttribute(getLine_LineAt(), ecorePackage.getELong(), "lineAt",
				null, 0, 1, Line.class, !IS_TRANSIENT, !IS_VOLATILE,
				IS_CHANGEABLE, IS_UNSETTABLE, !IS_ID, IS_UNIQUE, !IS_DERIVED,
				IS_ORDERED);
		initEAttribute(getLine_TapLastAt(), ecorePackage.getEInt(),
				"tapLastAt", null, 0, 1, Line.class, !IS_TRANSIENT,
				!IS_VOLATILE, IS_CHANGEABLE, IS_UNSETTABLE, !IS_ID, IS_UNIQUE,
				!IS_DERIVED, IS_ORDERED);
		initEAttribute(getLine_Br(), ecorePackage.getEInt(), "br", "0", 0, 1,
				Line.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE,
				!IS_UNSETTABLE, !IS_ID, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);
		initEAttribute(getLine_BrIn(), ecorePackage.getEInt(), "brIn", "0", 0,
				1, Line.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE,
				!IS_UNSETTABLE, !IS_ID, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);
		initEAttribute(getLine_BrOut(), ecorePackage.getEInt(), "brOut", "0",
				0, 1, Line.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE,
				!IS_UNSETTABLE, !IS_ID, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);
		initEAttribute(getLine_Bsent(), ecorePackage.getEInt(), "bsent", "0",
				0, 1, Line.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE,
				!IS_UNSETTABLE, !IS_ID, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);
		initEAttribute(getLine_LineId(), ecorePackage.getEInt(), "lineId",
				null, 0, 1, Line.class, !IS_TRANSIENT, !IS_VOLATILE,
				IS_CHANGEABLE, IS_UNSETTABLE, !IS_ID, IS_UNIQUE, !IS_DERIVED,
				IS_ORDERED);
		initEAttribute(getLine_Visible(), ecorePackage.getEBoolean(),
				"visible", "false", 0, 1, Line.class, !IS_TRANSIENT,
				!IS_VOLATILE, IS_CHANGEABLE, !IS_UNSETTABLE, !IS_ID, IS_UNIQUE,
				!IS_DERIVED, IS_ORDERED);
		initEAttribute(getLine_Advertised(), ecorePackage.getEBoolean(),
				"advertised", "false", 0, 1, Line.class, !IS_TRANSIENT,
				!IS_VOLATILE, IS_CHANGEABLE, !IS_UNSETTABLE, !IS_ID, IS_UNIQUE,
				!IS_DERIVED, IS_ORDERED);
		initEReference(getLine_Rules(), this.getTapRule(), null, "rules", null,
				0, -1, Line.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE,
				IS_COMPOSITE, !IS_RESOLVE_PROXIES, IS_UNSETTABLE, IS_UNIQUE,
				!IS_DERIVED, IS_ORDERED);
		initEAttribute(getLine_Session(), this.getIoSession(), "session", null,
				0, 1, Line.class, IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE,
				!IS_UNSETTABLE, !IS_ID, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);

		EOperation op = addEOperation(lineEClass, ecorePackage.getEBoolean(),
				"isRulesMatch", 0, 1, IS_UNIQUE, IS_ORDERED);
		addEParameter(op, this.getTelex(), "telex", 0, 1, IS_UNIQUE, IS_ORDERED);

		// Initialize data types
		initEDataType(endpointEDataType, InetSocketAddress.class, "Endpoint",
				IS_SERIALIZABLE, !IS_GENERATED_INSTANCE_CLASS);
		initEDataType(hashEDataType, Hash.class, "Hash", IS_SERIALIZABLE,
				!IS_GENERATED_INSTANCE_CLASS);
		initEDataType(ioSessionEDataType, IoSession.class, "IoSession",
				!IS_SERIALIZABLE, !IS_GENERATED_INSTANCE_CLASS);

		// Create resource
		createResource(eNS_URI);

		// Create annotations
		// JsonMetadata
		createJsonMetadataAnnotations();
	}

	/**
	 * Initializes the annotations for <b>JsonMetadata</b>.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	protected void createJsonMetadataAnnotations() {
		String source = "JsonMetadata";
		addAnnotation(getTelex_To(), source,
				new String[] { "keyType", "header" });
		addAnnotation(getTelex_End(), source, new String[] { "keyType",
				"signal" });
		addAnnotation(getTelex_Line(), source, new String[] { "keyType",
				"header" });
		addAnnotation(getTelex_Ring(), source, new String[] { "keyType",
				"header" });
		addAnnotation(getTelex_BytesReceived(), source, new String[] {
				"keyType", "header", "key", "br" });
		addAnnotation(getTelex_See(), source, new String[] { "keyType",
				"command" });
		addAnnotation(getTelex_Tap(), source, new String[] { "keyType",
				"command" });
	}

} //TelehashPackageImpl
