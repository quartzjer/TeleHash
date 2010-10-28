/**
 * <copyright>
 * </copyright>
 *
 * $Id$
 */
package org.telehash.model.impl;

import java.net.InetSocketAddress;

import org.eclipse.emf.ecore.EAttribute;
import org.eclipse.emf.ecore.EClass;
import org.eclipse.emf.ecore.EDataType;
import org.eclipse.emf.ecore.EPackage;
import org.eclipse.emf.ecore.impl.EPackageImpl;
import org.eclipse.emf.json.model.JsonPackage;
import org.telehash.Hash;
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
	private EDataType endpointEDataType = null;

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	private EDataType hashEDataType = null;

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
		return (EAttribute) telexEClass.getEStructuralFeatures().get(4);
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public EAttribute getTelex_BytesReceived() {
		return (EAttribute) telexEClass.getEStructuralFeatures().get(5);
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
		createEAttribute(telexEClass, TELEX__SEE);
		createEAttribute(telexEClass, TELEX__BYTES_RECEIVED);

		// Create data types
		endpointEDataType = createEDataType(ENDPOINT);
		hashEDataType = createEDataType(HASH);
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
		telexEClass.getESuperTypes().add(theJsonPackage.getJSObject());

		// Initialize classes and features; add operations and parameters
		initEClass(telexEClass, Telex.class, "Telex", !IS_ABSTRACT,
				!IS_INTERFACE, IS_GENERATED_INSTANCE_CLASS);
		initEAttribute(getTelex_To(), this.getEndpoint(), "to", null, 0, 1,
				Telex.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE,
				!IS_UNSETTABLE, !IS_ID, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);
		initEAttribute(getTelex_End(), this.getHash(), "end", null, 0, 1,
				Telex.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE,
				!IS_UNSETTABLE, !IS_ID, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);
		initEAttribute(getTelex_Line(), ecorePackage.getEInt(), "line", null,
				0, 1, Telex.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE,
				IS_UNSETTABLE, !IS_ID, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);
		initEAttribute(getTelex_Ring(), ecorePackage.getEInt(), "ring", null,
				0, 1, Telex.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE,
				IS_UNSETTABLE, !IS_ID, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);
		initEAttribute(getTelex_See(), this.getEndpoint(), "see", null, 0, -1,
				Telex.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE,
				!IS_UNSETTABLE, !IS_ID, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);
		initEAttribute(getTelex_BytesReceived(), ecorePackage.getEInt(),
				"bytesReceived", null, 0, 1, Telex.class, !IS_TRANSIENT,
				!IS_VOLATILE, IS_CHANGEABLE, !IS_UNSETTABLE, !IS_ID, IS_UNIQUE,
				!IS_DERIVED, IS_ORDERED);

		// Initialize data types
		initEDataType(endpointEDataType, InetSocketAddress.class, "Endpoint",
				IS_SERIALIZABLE, !IS_GENERATED_INSTANCE_CLASS);
		initEDataType(hashEDataType, Hash.class, "Hash", IS_SERIALIZABLE,
				!IS_GENERATED_INSTANCE_CLASS);

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
		addAnnotation(getTelex_See(), source, new String[] { "keyType",
				"command" });
		addAnnotation(getTelex_BytesReceived(), source, new String[] {
				"keyType", "header", "key", "_br" });
	}

} //TelehashPackageImpl
